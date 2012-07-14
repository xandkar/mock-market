%%%----------------------------------------------------------------------------
%%% Copyright (c) 2011-2012 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : market_ticker_sup.erl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : Mock Market ticker supervisor.
%%%----------------------------------------------------------------------------

-module(market_ticker_sup).
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
    Ticker = {
        market_ticker, {market_ticker, start_link, []},
        permanent, ?SHUTDOWN, worker, [market_ticker]
    },
    ServerSup = {
        market_server_sup, {market_server_sup, start_link, [LSock]},
        permanent, ?SHUTDOWN, supervisor, [market_server_sup]
    },
    BrokersSup = {
        market_broker_sup, {market_broker_sup, start_link, []},
        permanent, ?SHUTDOWN, supervisor, [market_broker_sup]
    },
    Children = [Ticker, ServerSup, BrokersSup],

    {ok, {?RESTART_STRATEGY, Children}}.


%% ============================================================================
%% Internal
%% ============================================================================
