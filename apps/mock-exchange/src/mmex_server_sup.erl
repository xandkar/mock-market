%%%----------------------------------------------------------------------------
%%% Copyright (c) 2011-2012 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : mmex_server_sup.erl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : Mock Market server supervisor.
%%%----------------------------------------------------------------------------

-module(mmex_server_sup).
-behaviour(supervisor).


%% API
-export([start_link/1
        ,start_child/0
        ]).

%% Callbacks
-export([init/1]).


-define(SUP_REF, ?MODULE).
-define(SHUTDOWN, brutal_kill).
-define(RESTART_STRATEGY, {simple_one_for_one, 5, 1}).


-include("mmex_config.hrl").


%% ============================================================================
%% API
%% ============================================================================

start_link(LSock) ->
    supervisor:start_link({local, ?SUP_REF}, ?MODULE, [LSock]).


start_child() ->
    supervisor:start_child(?SUP_REF, []).


%% ============================================================================
%% Callbacks
%% ============================================================================

init([LSock]) ->
    Server = {
        mmex_server, {mmex_server, start_link, [LSock]},
        transient, ?SHUTDOWN, worker, [mmex_server]
    },

    Children = [Server],

    {ok, {?RESTART_STRATEGY, Children}}.


%% ============================================================================
%% Internal
%% ============================================================================
