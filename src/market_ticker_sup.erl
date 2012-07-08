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
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).


-define(SHUTDOWN, 5000).


-include("market_config.hrl").


%% ============================================================================
%% API
%% ============================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% ============================================================================
%% Callbacks
%% ============================================================================

init([]) ->
    Ticker = {
        market_ticker, {market_ticker, start_link, []},
        permanent, ?SHUTDOWN, worker, [market_ticker]
    },
    BrokersSup = {
        market_broker_sup, {market_broker_sup, start_link, []},
        permanent, ?SHUTDOWN, supervisor, [market_broker_sup]
    },
    Children = [Ticker, BrokersSup],
    RestartStrategy = {one_for_one, 4, 3600},

    {ok, {RestartStrategy, Children}}.


%% ============================================================================
%% Internal
%% ============================================================================
