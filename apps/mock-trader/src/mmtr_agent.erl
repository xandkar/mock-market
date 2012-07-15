%%%----------------------------------------------------------------------------
%%% Copyright (c) 2011-2012 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : mmtr_agent.erl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : Agent process.
%%%----------------------------------------------------------------------------

-module(mmtr_agent).
-behaviour(gen_server).


%% API
-export([start_link/1]).

%% Callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).


%% Random choice from a randomly defined list of intervals :)
-define(INTERVAL, mmtr_lib:choice([75, 577, 975, 2671])).


-include("mmtr_config.hrl").
-include("mmtr_types.hrl").


-record(state, {name                   :: atom()
               ,portfolio = dict:new() :: dict()
               ,cashflow  = []         :: list()
               ,ticker_sock            :: gen_tcp:socket()
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
    gen_tcp:close(State#state.ticker_sock),
    {ok, State}.


code_change(_Old, State, _Other) ->
    {ok, State}.


handle_call(_Msg, _From, State) ->
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(init, State) ->
    {ok, Sock} = gen_tcp:connect("localhost", 7777, [binary]),
    schedule_next(get_quotes),
    {noreply, State#state{ticker_sock=Sock}, hibernate};

handle_info(get_quotes, #state{name=N, portfolio=P, cashflow=CF}=S) ->
    Quotes = get_quotes(S#state.ticker_sock),
    {Portfolio, CashFlow} = make_transaction(Quotes, N, P, CF),
    schedule_next(get_quotes),
    {noreply, S#state{portfolio=Portfolio, cashflow=CashFlow}, hibernate};

handle_info(Msg, State) ->
    io:format("UNEXPECTED MESSAGE:~n~p~n", [Msg]),
    {noreply, State}.


%% ============================================================================
%% Internal
%% ============================================================================

get_quotes(Sock) ->
    ok = gen_tcp:send(Sock, "\n"),
    receive
        {tcp, Sock, Data} -> binary_to_term(Data)
    end.


choose_transaction_type() ->
    TransactionTypes = [buy, sell],
    mmtr_lib:choice(TransactionTypes).


choose_amount_of_shares() ->
    PossibleAmounts = lists:seq(1, ?MAX_SHARES_PER_TRANSACTION),
    mmtr_lib:choice(PossibleAmounts).


schedule_next(Msg) ->
    erlang:send_after(?INTERVAL, self(), Msg).


%%-----------------------------------------------------------------------------
make_transaction(                [], _Name, P, CF) -> {P, CF};
make_transaction([{quotes, Quotes}],  Name, P, CF) ->
    % Decide what to do
    {Symbol, Price} = mmtr_lib:choice(Quotes),
    TransactionType = choose_transaction_type(),
    AmountOfShares = choose_amount_of_shares(),

    % Pack transaction data
    TransactionData = #transaction{
        timestamp = mmtr_lib:timestamp(),
        agent = Name,
        type = TransactionType,
        symbol = Symbol,
        amount = AmountOfShares,
        price = Price
    },

    % Update accumulated data
    {Portfolio, CashFlow} = transaction(P, CF, TransactionData),
    {Portfolio, CashFlow}.


%%-----------------------------------------------------------------------------
%% Function : transaction/3
%% Purpose  : Updates portfolio dict and appends to cash flow list in
%%            accordance with transaction data.
%%-----------------------------------------------------------------------------
transaction(Portfolio, CashFlow, #transaction{type=Type
                                             ,symbol=Symbol
                                             ,price=Price
                                             ,amount=Amount
                                             }=TransactionData) ->
    InitialShares = 0,
    UpdateFun = transaction_fun(Type, Amount),
    NewPortfolio = dict:update(Symbol, UpdateFun, InitialShares, Portfolio),
    NewCashFlow = [cash_value(Type, Amount, Price) | CashFlow],
    mmtr_scribe:log_transaction(TransactionData),
    {NewPortfolio, NewCashFlow}.


transaction_fun(buy,  NewAmount) -> fun(Current) -> Current + NewAmount end;
transaction_fun(sell, NewAmount) -> fun(Current) -> Current - NewAmount end.


cash_value(buy,  Amount, Price) -> -(Amount * Price);
cash_value(sell, Amount, Price) -> +(Amount * Price).
