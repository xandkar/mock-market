%%%----------------------------------------------------------------------------
%%% Copyright (c) 2011-2012 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : mmtr_sup.erl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : Mock Market Trader supervisor.
%%%----------------------------------------------------------------------------

-module(mmtr_sup).
-behaviour(supervisor).


%% API
-export([start_link/0]).

%% Callbacks
-export([init/1]).


-define(TYPE, worker).
-define(SHUTDOWN, 5000).
-define(RESTART, permanent).
-define(RESTART_STRATEGY, {one_for_one, 4, 3600}).


-include("mmtr_config.hrl").


%% ============================================================================
%% API
%% ============================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% ============================================================================
%% Callbacks
%% ============================================================================

init([]) ->
    Scribe = {
        mmtr_scribe, {mmtr_scribe, start_link, []},
        permanent, ?SHUTDOWN, worker, [mmtr_scribe]
    },

    AgentIDs = mmtr_lib:atoms_sequence("agent", "_", 1, ?NUM_AGENTS),
    Agents = spec_agents(AgentIDs),

    Children = [Scribe | Agents],
    {ok, {?RESTART_STRATEGY, Children}}.


%% ============================================================================
%% Internal
%% ============================================================================

spec_agents(IDs) -> spec_agents(IDs, []).

spec_agents([], Specs) -> Specs;
spec_agents([ID | IDs], Specs) ->
    M = mmtr_agent,
    F = start_link,
    A = [ID],
    Spec = {ID, {M, F, A}, ?RESTART, ?SHUTDOWN, ?TYPE, [M]},
    spec_agents(IDs, [Spec | Specs]).
