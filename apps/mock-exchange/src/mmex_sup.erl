%%%----------------------------------------------------------------------------
%%% Copyright (c) 2011-2012 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : market_sup.erl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : Mock Market supervisor.
%%%----------------------------------------------------------------------------

-module(market_sup).
-behaviour(supervisor).


%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).


-define(SHUTDOWN, 5000).
-define(RESTART_STRATEGY, {rest_for_one, 4, 3600}).


-include("market_config.hrl").


%% ============================================================================
%% API
%% ============================================================================

start_link(LSock) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [LSock]).


%% ============================================================================
%% Callbacks
%% ============================================================================

init([LSock]) ->
    Scribe = {
        market_scribe, {market_scribe, start_link, []},
        permanent, ?SHUTDOWN, worker, [market_scribe]
    },
    TickerSup = {
        market_ticker_sup, {market_ticker_sup, start_link, [LSock]},
        permanent, ?SHUTDOWN, supervisor, [market_ticker_sup]
    },
    Children = [Scribe, TickerSup],

    {ok, {?RESTART_STRATEGY, Children}}.


%% ============================================================================
%% Internal
%% ============================================================================
