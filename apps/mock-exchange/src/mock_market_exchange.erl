%%%----------------------------------------------------------------------------
%%% Copyright (c) 2011-2012 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : mock_market_exchange.erl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : Simple stock market simulation.
%%%----------------------------------------------------------------------------

-module(mock_market_exchange).
-behaviour(application).


-define(APPLICATION_NAME, mock_market_exchange).


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
    case mmex_sup:start_link(LSock) of
        {ok, Pid} ->
            mmex_scribe:register_with_logger(),
            mmex_scribe:add_handler(),
            mmex_server_sup:start_child(),
            {ok, Pid};

        Error ->
            {error, Error}
    end.


stop(_State) ->
    ok.
