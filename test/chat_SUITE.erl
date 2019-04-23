-module(chat_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("chat.hrl").

suite() ->
    [{timetrap, {seconds, 30}}].

init_per_suite(Config) ->
    application:start(chat),

    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, _Config) ->
    ok.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    [].
all() ->
    [user_registration,
     send_message
    ].

user_registration(_Config) ->
    Name = shahen,
    {_, P} = chat_register:user_register(Name),

    chat_register:delete_user(P).

send_message(_Config) ->
    Name1 = shahen,
    Name2 = shahena,

    {_, P1} = chat_register:user_register(Name1),
    {_, P2} = chat_register:user_register(Name2),

    ?LOG_DEBUG("1 --> ~p, p1 --> ~p", [chat_register:send_message(Name1, hello), P1]),
    ?LOG_DEBUG("2 --> ~p, p2 --> ~p", [chat_register:send_message(Name2, hello), P2]),
    ?LOG_DEBUG("3 --> ~p, p2 --> ~p", [chat_register:send_message(unknown, hello), P2]),

    chat_register:delete_user(P1),
    chat_register:delete_user(P2).
