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
-import(market_lib,
    [
        atoms_sequence/4,
        choice/1,
        random_symbol/0,
        random_price/0,
        transaction/3,
        timestamp/0
    ]
).

-include("market_config.hrl").


%%-----------------------------------------------------------------------------
%% Function : ticker/0
%% Purpose  : Generates a set of listings and starts ticker/1.
%%-----------------------------------------------------------------------------
ticker() ->
    % Generate listings
    Listings = sets:to_list(sets:from_list(
        [random_symbol() || _ <- lists:seq(1, ?NUM_LISTINGS)]
    )),

    ticker(Listings).


%%-----------------------------------------------------------------------------
%% Function : ticker/1
%% Purpose  : Announces current prices to brokers.
%%-----------------------------------------------------------------------------
ticker(Listings) ->
    Brokers = atoms_sequence("broker", "_", 1, ?NUM_BROKERS),

    receive
        stop ->
            void;
        Other ->
            io:format("WARNING! Unexpected request: ~p~n", [Other]),
            ticker(Listings)
    after ?TICKER_INTERVAL ->
            Prices = [{Symbol, random_price()} || Symbol <- Listings],
            Message = {ticker, {prices, Prices}},

            % Broadcast prices to brokers
            lists:foreach(
                fun(Broker) ->
                        Broker ! Message
                end,
                Brokers
            ),

            ticker(Listings)
    end.


%%-----------------------------------------------------------------------------
%% Function : broker/0
%% Purpose  : Initializes empty data containers and starts broker/2.
%%-----------------------------------------------------------------------------
broker() ->
    Portfolio = dict:new(),
    Transactions = [],
    broker(Portfolio, Transactions).


%%-----------------------------------------------------------------------------
%% Function : broker/2
%% Purpose  : Receives current prices and either buys or sells.
%%-----------------------------------------------------------------------------
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
            scribe_proc ! {
                ProcName,
                {timestamp, timestamp()},
                {transaction_data, TransactionData}
            },

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
        {
            ProcName,
            {timestamp, TimeStamp},
            {transaction_data, TransactionData}
        } ->

            {TransactionType, Symbol, Price, NumberOfShares} = TransactionData,

            LogEntryData = [
                float_to_list(TimeStamp),
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
