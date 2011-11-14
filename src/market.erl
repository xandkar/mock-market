%%%----------------------------------------------------------------------------
%%% Copyright (c) 2011 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : market.erl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : Simple stock market simulation.
%%%----------------------------------------------------------------------------

-module(market).
-export([start/0, stop/0]).


-include("market_config.hrl").


%%-----------------------------------------------------------------------------
%% Function : start/0
%% Purpose  : Starts the simulation.
%%-----------------------------------------------------------------------------
start() ->
    % Register & spawn scribe
    register(scribe_proc, spawn(market_agents, scribe, [])),

    % Register & spawn brokers
    lists:foreach(
        fun(BrokerName) ->
            register(BrokerName, spawn(market_agents, broker, []))
        end,
        market_lib:atoms_sequence("broker", "_", 1, ?NUM_BROKERS)
    ),

    % Register & spawn ticker
    register(ticker_proc, spawn(market_agents, ticker, [])).


%%-----------------------------------------------------------------------------
%% Function : stop/0
%% Purpose  : Stops the simulation.
%%-----------------------------------------------------------------------------
stop() ->
    Procs = [ticker_proc]
            ++ market_lib:atoms_sequence("broker", "_", 1, ?NUM_BROKERS)
            ++ [scribe_proc],

    lists:foreach(fun(Proc) -> Proc ! stop end, Procs).
