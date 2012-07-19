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


quote_to_mixmsg(Q) ->
    proplist_to_mixmsg("quote", quote_to_proplist(Q)).


quote_to_proplist({S, P}) ->
    [{symbol, S}, {price, P}].


proplist_to_mixmsg(Type, Props) ->
    MIXType = "type"++"="++Type,
    MIXProps = [term_to_list(K)++"="++term_to_list(V) || {K, V} <- Props],
    MIXMsg = string:join([MIXType | MIXProps], "|"),
    MIXMsg.


term_to_list(T) when is_list(T) -> T;
term_to_list(T) when is_atom(T) -> atom_to_list(T);
term_to_list(T) when is_integer(T) -> integer_to_list(T);
term_to_list(T) when is_float(T) -> float_to_string(2, T).


float_to_string(Precision, Float) ->
    io_lib:format("~."++integer_to_list(Precision)++"f", [Float]).
