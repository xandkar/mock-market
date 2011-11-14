%%%----------------------------------------------------------------------------
%%% Copyright (c) 2011 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : market_agents.erl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : Main interacting processes of the simulation.
%%%----------------------------------------------------------------------------

-module(market_agents).
-export([ticker/0, broker/0, scribe/0]).


-include("market_config.hrl").
-include("market_types.hrl").


%%-----------------------------------------------------------------------------
%% Function : ticker/0
%% Purpose  : Generates a set of listings and starts ticker/2.
%%-----------------------------------------------------------------------------
ticker() ->
    % Generate listings
    Listings = sets:to_list(sets:from_list(
        [market_lib:random_symbol() || _ <- lists:seq(1, ?NUM_LISTINGS)]
    )),

    % Enumerate broker names
    Brokers = market_lib:atoms_sequence("broker", "_", 1, ?NUM_BROKERS),

    ticker(Listings, Brokers).


%%-----------------------------------------------------------------------------
%% Function : ticker/2
%% Purpose  : Announces current prices to brokers.
%%-----------------------------------------------------------------------------
ticker(Listings, Brokers) ->
    receive
        stop ->
            void;

        Other ->
            io:format("WARNING! Unexpected request: ~p~n", [Other]),
            ticker(Listings, Brokers)

    after ?TICKER_INTERVAL ->
        Prices = [{Symbol, market_lib:random_price()} || Symbol <- Listings],
        Message = {ticker, {prices, Prices}},

        % Broadcast prices to brokers
        lists:foreach(
            fun(Broker) ->
                    Broker ! Message
            end,
            Brokers
        ),

        ticker(Listings, Brokers)
    end.


%%-----------------------------------------------------------------------------
%% Function : broker/0
%% Purpose  : Initializes empty data containers and starts broker/3.
%%-----------------------------------------------------------------------------
broker() ->
    Portfolio = dict:new(),
    CashFlow = [],
    {registered_name, Name} = erlang:process_info(self(), registered_name),
    broker(Name, Portfolio, CashFlow).


%%-----------------------------------------------------------------------------
%% Function : broker/3
%% Purpose  : Receives current prices and either buys or sells.
%%-----------------------------------------------------------------------------
broker(Name, Portfolio, CashFlow) ->
    receive
        {ticker, {prices, Prices}} ->

            % Decide what to do
            {Symbol, Price} = market_lib:choice(Prices),
            TransactionType = market_lib:choice([buy, sell]),
            NumberOfShares = market_lib:choice(
                lists:seq(1, ?MAX_SHARES_PER_TRANSACTION)
            ),

            % Pack transaction data
            TransactionData = #transaction{
                timestamp = market_lib:timestamp(),
                broker = Name,
                type = TransactionType,
                symbol = Symbol,
                shares = NumberOfShares,
                price = Price
            },

            % Update accumulated data
            {
                {portfolio, NewPortfolio}, {cashflow, NewCashFlow}
            } = market_lib:transaction(
                TransactionData,
                Portfolio,
                CashFlow
            ),

            % Log transaction to file
            scribe_proc ! {transaction, TransactionData},

            % Print accumulated values to stdout
            NewPortfolioList = dict:to_list(NewPortfolio),
            CashBalance = lists:sum(CashFlow),
            io:format("~p PORTFOLIO:~p~n", [Name, NewPortfolioList]),
            io:format("~p CASH BALANCE:~p~n", [Name, CashBalance]),
            io:format("~n"),

            % Now do it all over again! :)
            broker(Name, NewPortfolio, NewCashFlow);

        stop ->
            void;

        Other ->
            io:format("WARNING! Unexpected message: ~p~n", [Other]),
            broker(Name, Portfolio, CashFlow)
    end.


%%-----------------------------------------------------------------------------
%% Function : scribe/0
%% Purpose  : Opens log file for writing and starts scribe/1.
%%-----------------------------------------------------------------------------
scribe() ->
    file:make_dir(?PATH_DIR__DATA),
    {ok, LogFile} = file:open(?PATH_FILE__LOG, write),
    scribe(LogFile).


%%-----------------------------------------------------------------------------
%% Function : scribe/1
%% Purpose  : Receives transaction data and writes it to log file.
%%-----------------------------------------------------------------------------
scribe(LogFile) ->
    receive
        {transaction, TransactionData} ->

            LogEntry = string:join(
                [
                    float_to_list(TransactionData#transaction.timestamp),
                    atom_to_list(TransactionData#transaction.broker),
                    atom_to_list(TransactionData#transaction.type),
                    TransactionData#transaction.symbol,
                    integer_to_list(TransactionData#transaction.shares),
                    float_to_list(TransactionData#transaction.price)
                ],
                ?LOG_FIELD_DELIMITER
            ),

            io:format(LogFile, "~s~n", [LogEntry]),
            scribe(LogFile);

        stop ->
            file:close(LogFile);

        Other ->
            io:format("WARNING! Unexpected message: ~p~n", [Other]),
            scribe(LogFile)
    end.
