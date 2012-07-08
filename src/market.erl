%%%----------------------------------------------------------------------------
%%% Copyright (c) 2011-2012 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : market.erl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : Simple stock market simulation.
%%%----------------------------------------------------------------------------

-module(market).
-behaviour(application).


%% Application callbacks
-export([start/2, stop/1]).


%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case market_sup:start_link() of
        {ok, Pid} ->
            market_scribe:register_with_logger(),
            market_scribe:add_handler(),
            {ok, Pid};

        Error ->
            {error, Error}
    end.


stop(_State) ->
    ok.
