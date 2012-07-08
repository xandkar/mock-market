%%%----------------------------------------------------------------------------
%%% Copyright (c) 2011-2012 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : market_ticker.erl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : Ticker process.
%%%----------------------------------------------------------------------------

-module(market_ticker).
-behaviour(gen_server).


%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).


-define(SERVER_NAME, ?MODULE).


-include("market_config.hrl").
-include("market_types.hrl").


%% ============================================================================
%% API
%% ============================================================================

start_link() ->
    ServerName = {local, ?SERVER_NAME},
    Args = [],
    Opts = [],
    gen_server:start_link(ServerName, ?MODULE, Args, Opts).


%% ============================================================================
%% Callbacks
%% ============================================================================

init([]) ->
    self() ! init,
    Listings = [],
    Subscribers = [],
    State = {Listings, Subscribers},
    {ok, State}.


terminate(_Reason, State) ->
    {ok, State}.


code_change(_Old, State, _Other) ->
    {ok, State}.


handle_call(_Msg, _From, State) ->
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(init, {[], []}) ->
    % Generate listings
    Listings = sets:to_list(sets:from_list(
        [market_lib:random_symbol() || _ <- lists:seq(1, ?NUM_LISTINGS)]
    )),
    erlang:send_after(?TICKER_INTERVAL, self(), publish),
    {noreply, {Listings, []}};

handle_info({subscribe, PID}, {Listings, Subscribers}) ->
    {noreply, {Listings, [PID | Subscribers]}, hibernate};

handle_info(publish, {Listings, Subscribers} = State) ->
    Prices = [{Symbol, market_lib:random_price()} || Symbol <- Listings],

    % Broadcast prices to subscribers
    ok = send_to_all(Subscribers, {ticker, {prices, Prices}}),

    erlang:send_after(?TICKER_INTERVAL, self(), publish),

    {noreply, State, hibernate};

handle_info(_Msg, State) ->
    {noreply, State}.


%% ============================================================================
%% Internal
%% ============================================================================
send_to_all([], _) -> ok;
send_to_all([P|Procs], Msg) ->
    P ! Msg,
    send_to_all(Procs, Msg).
