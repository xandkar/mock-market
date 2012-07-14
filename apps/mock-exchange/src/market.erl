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


-define(APPLICATION_NAME, mock_market).


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
    {ok, LSock} = gen_tcp:listen(7777, [{active, true}]),
    case market_sup:start_link(LSock) of
        {ok, Pid} ->
            market_scribe:register_with_logger(),
            market_scribe:add_handler(),
            market_server_sup:start_child(),
            {ok, Pid};

        Error ->
            {error, Error}
    end.


stop(_State) ->
    ok.
