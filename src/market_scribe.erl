%%%----------------------------------------------------------------------------
%%% Copyright (c) 2011-2012 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : market_scribe.erl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : Scribe process.
%%%----------------------------------------------------------------------------

-module(market_scribe).
-behaviour(gen_event).


%% API
-export([start_link/0
        ,register_with_logger/0
        ,add_handler/0
        ,delete_handler/0
        ,log_transaction/1
        ]).

%% Callbacks
-export([init/1
        ,terminate/2
        ,code_change/3
        ,handle_event/2
        ,handle_call/2
        ,handle_info/2
        ]).


-include("market_config.hrl").
-include("market_types.hrl").


%% ============================================================================
%% API
%% ============================================================================

start_link() ->
    gen_event:start_link({local, ?MODULE}).


register_with_logger() ->
    error_logger:add_report_handler(?MODULE).


add_handler() ->
    gen_event:add_handler(?MODULE, ?MODULE, []).


delete_handler() ->
    gen_event:delete_handler(?MODULE, ?MODULE, []).


log_transaction(Data) ->
    gen_event:notify(?MODULE, {transaction, Data}).


%% ============================================================================
%% Callbacks
%% ============================================================================

init([]) ->
    file:make_dir(?PATH_DIR__DATA),
    {ok, LogFile} = file:open(?PATH_FILE__LOG, write),
    {ok, LogFile}.


terminate(_Reason, LogFile) ->
    file:close(LogFile),
    {ok, LogFile}.


code_change(_Old, State, _Other) ->
    {ok, State}.


handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.


handle_info(_Info, State) ->
    {ok, State}.


handle_event({transaction, Data}, LogFile) ->
    LogEntry = string:join(
        [
            float_to_list(Data#transaction.timestamp),
            atom_to_list(Data#transaction.broker),
            atom_to_list(Data#transaction.type),
            Data#transaction.symbol,
            integer_to_list(Data#transaction.shares),
            float_to_list(Data#transaction.price)
        ],
        ?LOG_FIELD_DELIMITER
    ),
    io:format(LogFile, "~s~n", [LogEntry]),
    {ok, LogFile};

handle_event(_Event, LogFile) ->
    {ok, LogFile}.

%% ============================================================================
%% Internal
%% ============================================================================
