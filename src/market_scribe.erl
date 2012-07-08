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


-define(EVENT_MGR_REF, ?MODULE).
-define(HANDLER, ?MODULE).


-include("market_config.hrl").
-include("market_types.hrl").


-record(state, {log_file :: pid()}).


%% ============================================================================
%% API
%% ============================================================================

start_link() ->
    EventMgrName = {local, ?EVENT_MGR_REF},
    gen_event:start_link(EventMgrName).


register_with_logger() ->
    error_logger:add_report_handler(?HANDLER).


add_handler() ->
    Args = [],
    gen_event:add_handler(?EVENT_MGR_REF, ?HANDLER, Args).


delete_handler() ->
    Args = [],
    gen_event:delete_handler(?EVENT_MGR_REF, ?HANDLER, Args).


log_transaction(Data) ->
    Event = {transaction, Data},
    gen_event:notify(?EVENT_MGR_REF, Event).


%% ============================================================================
%% Callbacks
%% ============================================================================

init([]) ->
    file:make_dir(?PATH_DIR__DATA),
    {ok, LogFile} = file:open(?PATH_FILE__LOG, write),
    {ok, #state{log_file=LogFile}}.


terminate(_Reason, #state{log_file=LogFile}=State) ->
    file:close(LogFile),
    {ok, State}.


code_change(_Old, State, _Other) ->
    {ok, State}.


handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.


handle_info(_Info, State) ->
    {ok, State}.


handle_event({transaction, Data}, #state{log_file=LogFile}=State) ->
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
    {ok, State};

handle_event(_Event, State) ->
    {ok, State}.


%% ============================================================================
%% Internal
%% ============================================================================
