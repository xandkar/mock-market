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
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).


-include("market_config.hrl").
-include("market_types.hrl").


-record(state, {
     name                   :: atom()
    ,portfolio = dict:new() :: dict()
    ,cashflow  = []         :: list()
}).


%% ============================================================================
%% API
%% ============================================================================

start_link(Name) ->
    ServerName = {local, Name},
    Args = [Name],
    Opts = [],
    gen_server:start_link(ServerName, ?MODULE, Args, Opts).


%% ============================================================================
%% Callbacks
%% ============================================================================

init([Name]) ->
    self() ! init,
    {ok, #state{name=Name}}.


terminate(_Reason, State) ->
    {ok, State}.


code_change(_Old, State, _Other) ->
    {ok, State}.


handle_call(_Msg, _From, State) ->
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(init, State) ->
    market_ticker ! {subscribe, self()},
    {noreply, State, hibernate};

handle_info({ticker, {prices, Prices}}, #state{name=Name}=State) ->
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
    {{portfolio, Portfolio}, {cashflow, CashFlow}} = market_lib:transaction(
        TransactionData,
        State#state.portfolio,
        State#state.cashflow
    ),

    market_scribe:log_transaction(TransactionData),

    % Print accumulated values to stdout
    %io:format("~p PORTFOLIO:~p~n", [Name, dict:to_list(Portfolio)]),
    %io:format("~p CASH BALANCE:~p~n", [Name, lists:sum(CashFlow)]),
    %io:format("~n"),

    {noreply
    ,#state{name=Name, portfolio=Portfolio, cashflow=CashFlow}
    ,hibernate
    };

handle_info(Msg, State) ->
    io:format("UNEXPECTED MESSAGE:~n~p~n", [Msg]),
    {noreply, State}.


%% ============================================================================
%% Internal
%% ============================================================================
