%%%----------------------------------------------------------------------------
%%% Copyright (c) 2011-2012 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : market.erl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : Mock Market supervisor.
%%%----------------------------------------------------------------------------

-module(market_sup).
-behaviour(supervisor).


%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).


-define(TYPE, worker).
-define(SHUTDOWN, 5000).
-define(RESTART, temporary).


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
    BrokerIDs = market_lib:atoms_sequence("broker", "_", 1, ?NUM_BROKERS),
    ChildrenBrokers = spec_brokers(BrokerIDs),

    ChildScribe = {
        scribe_proc,
        {market_scribe, start_link, [scribe_proc]},
        ?RESTART, ?SHUTDOWN, ?TYPE, [market_scribe]
    },

    ChildTicker = {
        ticker_proc,
        {market_ticker, start_link, [ticker_proc, BrokerIDs]},
        ?RESTART, ?SHUTDOWN, ?TYPE, [market_ticker]
    },

    Children = [ChildScribe, ChildTicker | ChildrenBrokers],
    RestartStrategy = {one_for_one, 4, 3600},

    {ok, {RestartStrategy, Children}}.


%% ============================================================================
%% Internal
%% ============================================================================

spec_brokers(IDs) -> spec_brokers(IDs, []).

spec_brokers([], Specs) -> Specs;
spec_brokers([ID | IDs], Specs) ->
    M = market_broker,
    F = start_link,
    A = [ID],
    Spec = {ID, {M, F, A}, ?RESTART, ?SHUTDOWN, ?TYPE, [M]},
    spec_brokers(IDs, [Spec | Specs]).
