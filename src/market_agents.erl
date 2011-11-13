%%%----------------------------------------------------------------------------
%%% Copyright (c) 2011 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : market_agents.erl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : Main interacting processes of the simulation.
%%%----------------------------------------------------------------------------

-module(market_agents).
-export([ticker/2, broker/0, scribe/0]).

-include("market.hrl").


%%-----------------------------------------------------------------------------
%% Function : ticker/2
%% Purpose  : Announces current prices to brokers.
%%-----------------------------------------------------------------------------
ticker(Listings, Interval) ->
    Brokers = market_lib:atoms_sequence("broker", "_", 1, ?NUM_BROKERS),

    receive
        stop ->
            void;
        Other ->
            io:format("WARNING! Unexpected request: ~p~n", [Other]),
            ticker(Listings, Interval)
    after Interval ->
            Prices = [{Symbol, market_lib:random_price()} || Symbol <- Listings],
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
            {Symbol, Price} = market_lib:choice(Prices),
            TransactionType = market_lib:choice([buy, sell]),
            NumberOfShares = market_lib:choice(lists:seq(1, ?MAX_SHARES_PER_TRANSACTION)),

            TransactionData = {
                TransactionType,
                Symbol,
                Price,
                NumberOfShares
            },

            % Perform transaction
            {NewPortfolio, NewTransactions} = market_lib:transaction(
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
