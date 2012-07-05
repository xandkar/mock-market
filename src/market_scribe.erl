%%%----------------------------------------------------------------------------
%%% Copyright (c) 2011-2012 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : market_scribe.erl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : Scribe process.
%%%----------------------------------------------------------------------------

-module(market_scribe).
-behaviour(gen_server).


%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-include("market_config.hrl").
-include("market_types.hrl").


%% ============================================================================
%% API
%% ============================================================================

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).


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


handle_call(_Msg, _From, State) ->
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({transaction, TransactionData}, LogFile) ->
    LogEntry = string:join(
        [
            float_to_list(TransactionData#transaction.timestamp),
            atom_to_list(TransactionData#transaction.broker),
            atom_to_list(TransactionData#transaction.type),
            TransactionData#transaction.symbol,
            integer_to_list(TransactionData#transaction.shares),
            float_to_list(TransactionData#transaction.price)
        ],
        ?LOG_FIELD_DELIMITER
    ),
    io:format(LogFile, "~s~n", [LogEntry]),
    {noreply, LogFile, hibernate};

handle_info(_Msg, State) ->
    {noreply, State}.


%% ============================================================================
%% Internal
%% ============================================================================
