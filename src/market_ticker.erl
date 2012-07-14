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


-record(state,
    {listings    = [] :: list()
    ,subscribers = [] :: list()
    }
).


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
    {ok, #state{}}.


terminate(_Reason, State) ->
    {ok, State}.


code_change(_Old, State, _Other) ->
    {ok, State}.


handle_call(_Msg, _From, State) ->
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(init, State) ->
    % Generate listings
    Listings = sets:to_list(sets:from_list(
        [market_lib:random_symbol() || _ <- lists:seq(1, ?NUM_LISTINGS)]
    )),

    % Ensure DB table
    ok = db_ensure_table(),

    % Schedule next publish event
    erlang:send_after(?TICKER_INTERVAL, self(), publish),

    {noreply, State#state{listings=Listings}};

handle_info({subscribe, PID}, #state{subscribers=Subscribers}=State) ->
    {noreply, State#state{subscribers=[PID|Subscribers]}, hibernate};

handle_info(publish, #state{listings=Listings, subscribers=Subscribers}=State) ->
    Prices = [{Symbol, market_lib:random_price()} || Symbol <- Listings],

    % Update DB
    ok = db_update(prices, Prices),

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


db_update(prices, Prices) ->
    true = ets:insert(?TICKER_TABLE_ID, {prices, Prices}),
    ok.


db_ensure_table() ->
    db_ensure_table(?TICKER_TABLE_ID, ets:info(?TICKER_TABLE_ID)).


db_ensure_table(TableID, undefined) ->
    Options = [protected, named_table, set],
    TableID = ets:new(TableID, Options),
    ok;

db_ensure_table(_TableID, _Info) ->
    ok.
