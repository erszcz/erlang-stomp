-module(sql_test).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

-define(LEXER, sql_lexer).
-define(PARSER, sql_parser).

run() ->
    {ok, Tokens, _EndLine} = ?LEXER:string("A = 22 or B < 10"),
    {ok, ParseTree} = ?PARSER:parse(Tokens),
    ?assertEqual({union,{predicate,{var,'A'},'=',22},
                  {predicate,{var,'B'},'<',10}},
                 ParseTree).
