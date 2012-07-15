%%%----------------------------------------------------------------------------
%%% Copyright (c) 2011-2012 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : mmex_ticker_sup.erl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : Mock Market ticker supervisor.
%%%----------------------------------------------------------------------------

-module(mmex_ticker_sup).
-behaviour(supervisor).


%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).


-define(SHUTDOWN, 5000).
-define(RESTART_STRATEGY, {rest_for_one, 4, 3600}).


-include("mmex_config.hrl").


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
        mmex_ticker, {mmex_ticker, start_link, []},
        permanent, ?SHUTDOWN, worker, [mmex_ticker]
    },
    ServerSup = {
        mmex_server_sup, {mmex_server_sup, start_link, [LSock]},
        permanent, ?SHUTDOWN, supervisor, [mmex_server_sup]
    },
    Children = [Ticker, ServerSup],

    {ok, {?RESTART_STRATEGY, Children}}.


%% ============================================================================
%% Internal
%% ============================================================================
