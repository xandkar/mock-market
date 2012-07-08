%%%----------------------------------------------------------------------------
%%% Copyright (c) 2011-2012 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : market_broker.erl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : Broker process.
%%%----------------------------------------------------------------------------

-module(market_broker).
-behaviour(gen_server).


%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-include("market_config.hrl").
-include("market_types.hrl").


%% ============================================================================
%% API
%% ============================================================================

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name], []).


%% ============================================================================
%% Callbacks
%% ============================================================================

init([Name]) ->
    self() ! init,
    {ok, {Name, [], []}}.


terminate(_Reason, State) ->
    {ok, State}.


code_change(_Old, State, _Other) ->
    {ok, State}.


handle_call(_Msg, _From, State) ->
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(init, {Name, [], []}) ->
    Portfolio = dict:new(),
    CashFlow = [],
    {noreply, {Name, Portfolio, CashFlow}, hibernate};

handle_info({ticker, {prices, Prices}}, {Name, Portfolio, CashFlow}) ->
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

    market_scribe:log_transaction(TransactionData),

    % Print accumulated values to stdout
    NewPortfolioList = dict:to_list(NewPortfolio),
    CashBalance = lists:sum(CashFlow),
    io:format("~p PORTFOLIO:~p~n", [Name, NewPortfolioList]),
    io:format("~p CASH BALANCE:~p~n", [Name, CashBalance]),
    io:format("~n"),

    {noreply, {Name, NewPortfolio, NewCashFlow}, hibernate};

handle_info(_Msg, State) ->
    {noreply, State}.


%% ============================================================================
%% Internal
%% ============================================================================
