%%%----------------------------------------------------------------------------
%%% Copyright (c) 2011-2012 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : mmex_server.erl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : TCP interface to the exchange.
%%%----------------------------------------------------------------------------

-module(mmex_server).
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


-include("mmex_config.hrl").


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
    mmex_server_sup:start_child(),
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
    Msgs = quotes_to_mixmsgs(ets:lookup(?TICKER_TABLE_ID, quotes)),
    gen_tcp:send(Socket, Msgs).


quotes_to_mixmsgs([]) -> "\n";
quotes_to_mixmsgs([{quotes, Quotes}]) ->
    string:join([quote_to_mixmsg(Q) || Q <- Quotes], "\n").


float_to_string(Precision, Float) ->
    io_lib:format("~."++integer_to_list(Precision)++"f", [Float]).


quote_to_mixmsg({S, P}) ->
    Price = float_to_string(2, P),
    string:join(["msg_type=quote", "symbol="++S, "price="++Price], "|").
