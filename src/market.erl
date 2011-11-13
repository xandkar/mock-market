%%%----------------------------------------------------------------------------
%%% Copyright (c) 2011 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : market.erl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : Simple stock market simulation.
%%%----------------------------------------------------------------------------

-module(market).
-export([start/0, stop/0, ticker/2, broker/0, scribe/0]).

-include("market.hrl").


%%============================================================================
%% Controlls
%%============================================================================

%%-----------------------------------------------------------------------------
%% Function : start/0
%% Purpose  : Starts the simulation.
%%-----------------------------------------------------------------------------
start() ->

    % Generate listings
    Listings = sets:to_list(sets:from_list(
        [random_symbol() || _ <- lists:seq(1, ?NUM_LISTINGS)]
    )),

    % Register & spawn scribe
    register(scribe_proc, spawn(market, scribe, [])),

    % Register & spawn brokers
    lists:foreach(
        fun(BrokerName) ->
            register(BrokerName, spawn(market, broker, []))
        end,
        atoms_sequence("broker", "_", 1, ?NUM_BROKERS)
    ),

    % Register & spawn ticker
    register(ticker_proc, spawn(market, ticker, [Listings, ?TICKER_INTERVAL])).


%%-----------------------------------------------------------------------------
%% Function : stop/0
%% Purpose  : Stops the simulation.
%%-----------------------------------------------------------------------------
stop() ->
    Procs = [ticker_proc]
            ++ atoms_sequence("broker", "_", 1, ?NUM_BROKERS)
            ++ [scribe_proc],

    lists:foreach(fun(Proc) -> Proc ! stop end, Procs).


%%============================================================================
%% Agents
%%============================================================================

%%-----------------------------------------------------------------------------
%% Function : ticker/2
%% Purpose  : Announces current prices to brokers.
%%-----------------------------------------------------------------------------
ticker(Listings, Interval) ->
    Brokers = atoms_sequence("broker", "_", 1, ?NUM_BROKERS),

    receive
        stop ->
            void;
        Other ->
            io:format("WARNING! Unexpected request: ~p~n", [Other]),
            ticker(Listings, Interval)
    after Interval ->
            Prices = [{Symbol, random_price()} || Symbol <- Listings],
            Message = {ticker, {prices, Prices}},

            % Broadcast prices to brokers
            lists:foreach(
                fun(Broker) ->
                        Broker ! Message
                end, 
                Brokers
            ),

            ticker(Listings, Interval)
    end.


%%-----------------------------------------------------------------------------
%% Function : broker/0
%% Purpose  : Receives current prices and either buys or sells.
%%-----------------------------------------------------------------------------
broker() ->
    Portfolio = dict:new(),
    Transactions = [],
    broker(Portfolio, Transactions).

broker(Portfolio, Transactions) ->
    {registered_name, ProcName} = erlang:process_info(self(), registered_name),
    CashBalance = lists:sum(Transactions),

    receive
        {ticker, {prices, Prices}} ->
            {Symbol, Price} = choice(Prices),
            TransactionType = choice([buy, sell]),
            NumberOfShares = choice(lists:seq(1, ?MAX_SHARES_PER_TRANSACTION)),

            TransactionData = {
                TransactionType,
                Symbol,
                Price,
                NumberOfShares
            },

            % Perform transaction
            {NewPortfolio, NewTransactions} = transaction(
                TransactionData,
                Portfolio,
                Transactions
            ),

            % Send to scribe for recording
            scribe_proc ! {ProcName, transaction_data, TransactionData},

            NewPortfolioList = dict:to_list(NewPortfolio),
            io:format("~p PORTFOLIO:~p~n", [ProcName, NewPortfolioList]),
            io:format("~p CASH BALANCE:~p~n", [ProcName, CashBalance]),
            io:format("~n"),

            broker(NewPortfolio, NewTransactions);
        stop ->
            void;
        Other ->
            io:format("WARNING! Unexpected message: ~p~n", [Other]),
            broker(Portfolio, Transactions)
    end.


%%-----------------------------------------------------------------------------
%% Function : scribe/0
%% Purpose  : Receives transaction data and writes it to log file.
%%-----------------------------------------------------------------------------
scribe() ->
    file:make_dir(?PATH_DIR__DATA),
    {ok, LogFile} = file:open(?PATH_FILE__LOG, write),
    scribe(LogFile).

scribe(LogFile) ->
    receive
        {ProcName, transaction_data, TransactionData} ->
            {TransactionType, Symbol, Price, NumberOfShares} = TransactionData,

            LogEntryData = [
                atom_to_list(ProcName),
                atom_to_list(TransactionType),
                Symbol,
                integer_to_list(NumberOfShares),
                float_to_list(Price)
            ],

            LogEntry = string:join(LogEntryData, ?LOG_FIELD_DELIMITER),
            io:format(LogFile, "~s~n", [LogEntry]),
            scribe(LogFile);
        stop ->
            file:close(LogFile);
        Other ->
            io:format("WARNING! Unexpected message: ~p~n", [Other]),
            scribe(LogFile)
    end.


%%============================================================================
%% Helpers
%%============================================================================

%%-----------------------------------------------------------------------------
%% Function : transaction/3
%% Purpose  : Modifies portfolio dict and transactions list in accordance with
%%            transaction data.
%%-----------------------------------------------------------------------------
transaction({Type, Symbol, Price, Shares}, Portfolio, PastTransactions) ->
    NewPortfolio = dict:update(
        Symbol,
        case Type of
            buy  -> fun(CurrentShares) -> CurrentShares + Shares end;
            sell -> fun(CurrentShares) -> CurrentShares - Shares end
        end,
        _InitialShares =
            case Type of
                buy  -> + Shares;
                sell -> - Shares
            end,
        Portfolio
    ),

    NewTransactions =
        case Type of
            buy  -> [-(Price * Shares) | PastTransactions];
            sell -> [+(Price * Shares) | PastTransactions]
        end,

    {NewPortfolio, NewTransactions}.


%%-----------------------------------------------------------------------------
%% Function : random_symbol/0
%% Purpose  : Generates a random stock symbol.
%%-----------------------------------------------------------------------------
random_symbol() ->
    CharPool = "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
    LenPool = [1, 2, 3, 4],
    Symbol = [choice(CharPool) || _ <- lists:seq(1, choice(LenPool))],
    Symbol.


%%-----------------------------------------------------------------------------
%% Function : random_price/0
%% Purpose  : Generates a random price.
%%-----------------------------------------------------------------------------
random_price() ->
    DigPool = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
    LenPool = [1, 2, 3],
    Length = choice(LenPool),

    Dollars = [integer_to_list(choice(DigPool)) || _ <- lists:seq(1, Length)],
    Cents   = [integer_to_list(choice(DigPool)) || _ <- lists:seq(1, 2)],

    Price = string:join(
        [string:join(Dollars, ""), string:join(Cents, "")],
        "."
    ),

    list_to_float(Price).


%%-----------------------------------------------------------------------------
%% Function : choice/1
%% Purpose  : Pick and return a random element from a given list.
%%-----------------------------------------------------------------------------
choice(List) ->
    reseed(),
    Maximum = length(List),
    Element = random:uniform(Maximum),
    lists:nth(Element, List).


%%-----------------------------------------------------------------------------
%% Function : reseed/0
%% Purpose  : Reseed pseudorandom number generator.
%%-----------------------------------------------------------------------------
reseed() ->
    random:seed(timehash(), timehash(), timehash()).


%%-----------------------------------------------------------------------------
%% Function : timestamp/0
%% Purpose  : Generates a Unix timestamp float.
%%-----------------------------------------------------------------------------
timestamp() ->
    [Mega, Sec, Micro] = [integer_to_list(I) || I <- tuple_to_list(now())],
    Seconds = string:join([Mega, Sec], ""),
    Timestamp = string:join([Seconds, Micro], "."),
    list_to_float(Timestamp).


%%-----------------------------------------------------------------------------
%% Function : timehash/0
%% Purpose  : Generates a cryptographically unique integer based on current
%%            time.
%%-----------------------------------------------------------------------------
timehash() ->
    Timestamp = float_to_list(timestamp()),
    HashBin  = crypto:sha(Timestamp),
    HashList = [integer_to_list(I) || I <- binary_to_list(HashBin)],
    HashStr  = string:join(HashList, ""),
    list_to_integer(HashStr).


%%-----------------------------------------------------------------------------
%% Function : atoms_sequence/4
%% Purpose  : Generate a list of numerically sequential atoms:
%%           [atom_1, atom_2, ...]
%%-----------------------------------------------------------------------------
atoms_sequence(String, Separator, FromNum, ToNum) ->
    [
        list_to_atom(
            string:join([String, integer_to_list(I)], Separator)
        ) ||
        I <- lists:seq(FromNum, ToNum)
    ].
