%%%----------------------------------------------------------------------------
%%% Copyright (c) 2011-2012 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : mock_market_trader.erl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : Simple stock market simulation.
%%%----------------------------------------------------------------------------

-module(mock_market_trader).
-behaviour(application).


-define(APPLICATION_NAME, mock_market_trader).


%% Application callbacks
-export([start/0
        ,start/2
        ,stop/1
        ]).


%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:start(?APPLICATION_NAME),
    ok.


start(_StartType, _StartArgs) ->
    case mmtr_sup:start_link() of
        {ok, Pid} ->
            mmtr_scribe:register_with_logger(),
            mmtr_scribe:add_handler(),
            {ok, Pid};

        Error ->
            {error, Error}
    end.


stop(_State) ->
    ok.
