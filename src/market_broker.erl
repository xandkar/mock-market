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

handle_info({ticker, {prices, Prices}}, State) ->
    % Decide what to do
    {Symbol, Price} = market_lib:choice(Prices),
    TransactionType = choose_transaction_type(),
    AmountOfShares = choose_amount_of_shares(),

    % Pack transaction data
    TransactionData = #transaction{
        timestamp = market_lib:timestamp(),
        broker = State#state.name,
        type = TransactionType,
        symbol = Symbol,
        amount = AmountOfShares,
        price = Price
    },

    % Update accumulated data
    P = State#state.portfolio,
    C = State#state.cashflow,
    {Portfolio, CashFlow} = transaction(P, C, TransactionData),
    market_scribe:log_transaction(TransactionData),

    {noreply, State#state{portfolio=Portfolio, cashflow=CashFlow}, hibernate};

handle_info(Msg, State) ->
    io:format("UNEXPECTED MESSAGE:~n~p~n", [Msg]),
    {noreply, State}.


%% ============================================================================
%% Internal
%% ============================================================================

choose_transaction_type() ->
    TransactionTypes = [buy, sell],
    market_lib:choice(TransactionTypes).


choose_amount_of_shares() ->
    PossibleAmounts = lists:seq(1, ?MAX_SHARES_PER_TRANSACTION),
    market_lib:choice(PossibleAmounts).


%%-----------------------------------------------------------------------------
%% Function : transaction/3
%% Purpose  : Updates portfolio dict and appends to cash flow list in
%%            accordance with transaction data.
%%-----------------------------------------------------------------------------
transaction(Portfolio, CashFlow, #transaction{type=Type
                                             ,symbol=Symbol
                                             ,price=Price
                                             ,amount=Amount
                                             }) ->
    InitialShares = 0,
    UpdateFun = transaction_fun(Type, Amount),
    NewPortfolio = dict:update(Symbol, UpdateFun, InitialShares, Portfolio),
    NewCashFlow = [cash_value(Type, Amount, Price) | CashFlow],

    {NewPortfolio, NewCashFlow}.


transaction_fun(buy,  NewAmount) -> fun(Current) -> Current + NewAmount end;
transaction_fun(sell, NewAmount) -> fun(Current) -> Current - NewAmount end.


cash_value(buy,  Amount, Price) -> -(Amount * Price);
cash_value(sell, Amount, Price) -> +(Amount * Price).
