%%%----------------------------------------------------------------------------
%%% Copyright (c) 2011-2012 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : market_server.erl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : TCP interface to the exchange.
%%%----------------------------------------------------------------------------

-module(market_server).
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


-include("market_config.hrl").
-include("market_types.hrl").


-record(state, {lsock :: gen_tcp:socket()}).


%% ============================================================================
%% API
%% ============================================================================

start_link(LSock) ->
    Args = [LSock],
    Opts = [],
    gen_server:start_link(?MODULE, Args, Opts).


%% ============================================================================
%% Callbacks
%% ============================================================================

init([LSock]) ->
    gen_server:cast(self(), init),
    {ok, #state{lsock=LSock}}.


terminate(_Reason, State) ->
    {ok, State}.


code_change(_Old, State, _Other) ->
    {ok, State}.


handle_call(_Msg, _From, State) ->
    {reply, ok, State}.


handle_cast(init, #state{lsock=LSock}=State) ->
    {ok, _Sock} = gen_tcp:accept(LSock),
    market_server_sup:start_child(),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({tcp, Socket, Data}, State) ->
    ok = handle_data(Socket, Data),
    {noreply, State};

handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};


handle_info(_Msg, State) ->
    {noreply, State}.


%% ============================================================================
%% Internal
%% ============================================================================

handle_data(Socket, _Data) ->
    [Prices] = ets:lookup(?TICKER_TABLE_ID, prices),
    Reply = term_to_binary(Prices),
    gen_tcp:send(Socket, Reply).
