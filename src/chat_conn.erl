-module(chat_conn).

-behaviour(gen_server).

%% API
-export([start/1,
         stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-include("chat.hrl").

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start(Name) ->
    gen_server:start({local, Name}, ?MODULE, [], []).

stop(Name) ->
    gen_server:stop(Name).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({chat_message, Message}, State) ->
    ?LOG_DEBUG("Receive --> ~p", [Message]),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
