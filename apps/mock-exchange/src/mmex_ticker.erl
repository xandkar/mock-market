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
    ok = db_ensure_table(),
    schedule_next(publish),
    {noreply, State#state{listings=generate_listings()}};

handle_info(publish, #state{listings=Listings}=State) ->
    ok = db_update(quotes, generate_quotes(Listings)),
    schedule_next(publish),
    {noreply, State, hibernate};

handle_info(_Msg, State) ->
    {noreply, State}.


%% ============================================================================
%% Internal
%% ============================================================================

schedule_next(Msg) ->
    erlang:send_after(?TICKER_INTERVAL, self(), Msg).


db_update(quotes, Quotes) ->
    true = ets:insert(?TICKER_TABLE_ID, {quotes, Quotes}),
    ok.


db_ensure_table() ->
    db_ensure_table(?TICKER_TABLE_ID, ets:info(?TICKER_TABLE_ID)).


db_ensure_table(TableID, undefined) ->
    Options = [protected, named_table, set],
    TableID = ets:new(TableID, Options),
    ok;

db_ensure_table(_TableID, _TableInfo) ->
    ok.


generate_listings() ->
    sets:to_list(sets:from_list(
        [market_lib:random_symbol() || _ <- lists:seq(1, ?NUM_LISTINGS)]
    )).


generate_quotes(Listings) ->
    [{Symbol, market_lib:random_price()} || Symbol <- Listings].
