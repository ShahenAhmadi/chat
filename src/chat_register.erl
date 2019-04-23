-module(chat_register).

-behaviour(gen_server).

%% API
-export([start_link/0,
         user_register/1,
         send_message/2,
         delete_user/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-include("chat.hrl").

-define(SERVER, ?MODULE).

-record(state, {user_lists :: list()}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

user_register(UserName) ->
    gen_server:call(?SERVER, {user_register, UserName}).


send_message(UserName, Message) ->
    gen_server:call(?SERVER, {send_message, UserName, Message}).

delete_user(UserName) ->
    gen_server:call(?SERVER, {delete_user, UserName}).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{user_lists = []}}.

handle_call({user_register, UserName}, _From,
            #state{user_lists = UserLists} = State) ->
    {ok, Pid} = chat_conn:start(UserName),
    {Reply, NewUserLists} = user_control(UserLists, UserName, Pid),
    NewReply = {Reply, Pid},
    NewState = State#state{user_lists = NewUserLists},

    {reply, NewReply, NewState};
handle_call({send_message, UserName, Message}, _From, #state{user_lists = UserLists} = State) ->
    %% Reply = case lists:keyfind(From, 2, UserLists) of
    %%             {_, From} ->
    %%                 case lists:keyfind(UserName, 1, UserLists) of
    %%                     {UserName, Sender} ->
    %%                         chat_conv:start_link(),
    %%                         UserName ! {Message, Sender},
    %%                         send;

    %%                     false ->
    %%                         user_not_exists
    %%                 end;
    %%             false ->
    %%                 you_not_register
    %%         end,
    Reply = case lists:keyfind(UserName, 1, UserLists) of
                {UserName, _} ->
                    UserName ! {chat_message, Message},
                    send;

                false ->
                    user_not_exists
            end,
    {reply, Reply, State};
handle_call({delete_user, UserName}, _From, #state{user_lists = UserLists} = State) ->
    {Reply, NewUserLists} = case delete_user(UserName, UserLists) of
                                {done, NewLists} ->
                                    Reply0 = chat_conn:stop(UserName),
                                    {Reply0, NewLists};

                                {Error, _} ->
                                    {Error, UserLists}
                            end,

    {reply, Reply, State#state{user_lists = NewUserLists}};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

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
user_control([], User, Pid) ->
    {done, [{User, Pid}]};
user_control(UserLists, User, Pid) ->
    case lists:keymember(User, 1, UserLists) of
        true ->
            {user_exists, UserLists};
        false ->
            {done, [{User, Pid} | UserLists]}
    end.

delete_user(UserName, UserLists) when is_atom(UserName) ->
    case lists:keyfind(UserName, 1, UserLists) of
        {UserName, _} ->
            {done, lists:keydelete(UserName, 1, UserLists)};
        false ->
            {not_exist, UserLists}
    end;
delete_user(UserPid, UserLists) when is_pid(UserPid) ->
    case lists:keyfind(UserPid, 2, UserLists) of
        {_, UserPid} ->
            {done, lists:keydelete(UserPid, 2, UserLists)};
        false ->
            {not_exist, UserLists}
    end;
delete_user(_, UserLists) ->
    {invalid_request, UserLists}.
