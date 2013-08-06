%% --------------------------
%% @copyright 2010 Bob.sh
%% @doc stomp test routines
%%
%% @end
%% @hidden
%% --------------------------

-module(stomp_tests).
-include_lib("eunit/include/eunit.hrl").


%% @doc Test connecting
%% @end
basic_connection_test() ->
    Conn = stomp:connect("localhost", 61613, "", ""),
    ?assert(is_port(Conn)),
    stomp:disconnect(Conn).

%% @doc Test subscribing
%% @end
basic_subscribe_test() ->
    Conn = stomp:connect("localhost", 61613, "", ""),
    ?assert(ok == stomp:subscribe("/queue/foobar", Conn)),
    stomp:disconnect(Conn).

%% @doc Test sending a message
%% @end
basic_send_test() ->
    Conn = stomp:connect("localhost", 61613, "", ""),
    ?assert(ok == stomp:send(Conn, "/queue/foobar", [], "hello world")),
    stomp:disconnect(Conn).

%% @doc Test parsing a few consecutive messages
%% @end
parser_test() ->
    {ok, Tokens, _EndLine} = stomp_lexer:string(messages()),
    {ok, ParseTree} = stomp_parser:parse(Tokens),
    Expected = [[{type, "CONNECTED"},
                 {headers, [{"version","1.2"}]},
                 {body, ""}],
                [{type, "MESSAGE"},
                 {headers, [{"subscription", "0"},
                            {"message-id", "007"},
                            {"destination", "/queue/a"},
                            {"content-type", "text/plain"}]},
                 {body, "hello queue a"}]],
    ?assertEqual(Expected, ParseTree).

messages() ->
    {ok, BContents} = file:read_file("stomp.txt"),
    binary_to_list(BContents).
