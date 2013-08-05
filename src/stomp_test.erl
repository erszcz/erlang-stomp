-module(stomp_test).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

-define(LEXER, stomp_lexer).
-define(PARSER, stomp_parser).

messages() ->
    {ok, BContents} = file:read_file("stomp.txt"),
    binary_to_list(BContents).

connected_test() ->
    {ok, Tokens, _EndLine} = ?LEXER:string(messages()),
    io:format("Tokens: ~p~n", [Tokens]),
    {ok, ParseTree} = ?PARSER:parse(Tokens),
    io:format("ParseTree: ~p~n", [ParseTree]).
