%% --------------------------
%% @author Ian Brown
%% @copyright 2009 Ian Brown,
%%            2010 Bob.sh,
%%            2013 (c) Erlang Solutions
%% @version 0.1
%% @doc Module for client support of the STOMP messaging protocol
%% ([http://stomp.codehaus.org/Protocol]).
%%
%% Originally authored by Ian Brown. Modified by Bob.sh.
%%
%% @end
%% --------------------------
-module(stomp).

-export([connect/4, %% "You sunk my scrabbleship!"
         disconnect/1,
         subscribe/2,
         subscribe/3,
         unsubscribe/2,
         get_messages/1,
         get_message_id/1,
         ack/2,
         ack/3,
         send/4,
         begin_transaction/2,
         commit_transaction/2,
         abort_transaction/2,
         on_message/2]).

-include("stomp.hrl").

%% @doc Connect to a STOMP server and return a `#stomp_conn{}'
%%
%% Example: Conn = stomp:connect("localhost", 61613, "", "").
%%
%% @end
connect(Host, PortNo, Login, Passcode)  ->
    Message = ["CONNECT", "\nlogin: ", Login, "\npasscode: ", Passcode,
               "\n\n", [0]],
    {ok, Sock} = gen_tcp:connect(Host, PortNo, [{active, false}]),
    gen_tcp:send(Sock, Message),
    {[[{type, Type}, _, _]], Conn} = get_messages(#stomp_conn{socket = Sock}),
    case Type of
        "CONNECTED" ->
            Sock;
        _ ->
            throw("Error occured during connection attempt.")
    end,
    Conn.

%% @doc Subscribe to a named queue
%%
%% Example: stomp:subscribe("/queue/foobar", Conn).
%%
%% @end
subscribe(Destination, Connection) ->
    subscribe(Destination, Connection, [{"ack", "auto"}]),
    ok.

%% @doc Subscribe to a named queue - with options.
%%
%% Example: stomp:subscribe("/queue/foobar", Conn, [{"ack", "client"}]).
%% Example: stomp:subscribe("/queue/foobar", Conn,
%%                          [{"ack", "client"},
%%                           {"activemq.prefetchSize", "1"}]).
%%
%% @end
subscribe(Destination, #stomp_conn{socket = Socket}, Options) ->
    Message = ["SUBSCRIBE", "\ndestination: ", Destination,
               concatenate_options(Options), "\n\n", [0]],
    gen_tcp:send(Socket, Message),
    ok.

%% @doc Remove an existing subscription
%%
%% Example: stomp:unsubscribe("/queue/foobar", Conn).
%%
%% @end
unsubscribe(Destination, #stomp_conn{socket = Socket}) ->
    Message = ["UNSUBSCRIBE", "\ndestination: ", Destination, "\n\n", [0]],
    gen_tcp:send(Socket, Message),
    ok.

%% @doc Disconnect gracefully from the existing connection
%%
%% Example: stomp:disconnect(Conn).
%%
%% @end
disconnect(#stomp_conn{socket = Socket}) ->
    Message = ["DISCONNECT", "\n\n", [0]],
    gen_tcp:send(Socket, Message),
    gen_tcp:close(Socket),
    ok.

%% @doc Get a particular message ID
%%
%% Example: stomp:get_message_id(Message).
%%
%% @end
get_message_id([_, {headers, Headers}, _]) ->
    get_message_id(Headers);
get_message_id([H|T]) ->
    case H of
        {"message-id", MessageId} ->
            MessageId;
        _ ->
            get_message_id(T)
    end;
get_message_id([]) ->
    throw("No header with name of 'message-id' was found.").

%% @doc Acknowledge consumption of a message
%%
%% Example: stomp:ack(Conn, Message).
%% Example: stomp:ack(Conn, stomp:get_message_id(Message)).
%% Example: stomp:ack(Conn, "ID:phosphorus-63844-1247442885553-3:1:1:1:1").
%%
%% @end
ack(Connection, [Type, Headers, Body]) ->
    MessageId = get_message_id([Type, Headers, Body]),
    ack(Connection, MessageId);
ack(#stomp_conn{socket = Socket}, MessageId) ->
    AckMessage = ["ACK", "\nmessage-id: ", MessageId, "\n\n", [0]],
    gen_tcp:send(Socket, AckMessage),
    ok.

%% @doc Acknowledge consumption of a message
%%
%% Example: stomp:ack(Conn, Message, TransactionId).
%% Example: stomp:ack(Conn, stomp:get_message_id(Message), TransactionId).
%% Example: stomp:ack(Conn, "ID:phosphorus-63844-1247442885553-3:1:1:1:1",
%%                    TransactionId).
%%
%% @end
ack(Connection, [Type, Headers, Body], TransactionId) ->
    MessageId = get_message_id([Type, Headers, Body]),
    ack(Connection, MessageId, TransactionId);
ack(#stomp_conn{socket = Socket}, MessageId, TransactionId) ->
    AckMessage = ["ACK", "\nmessage-id: ", MessageId,
                  "\ntransaction: ", TransactionId, "\n\n", [0]],
    gen_tcp:send(Socket, AckMessage),
    ok.

%% @doc Send a message to the destination in the messaging system
%%
%% Example: stomp:send(Conn, "/queue/foobar", [], "hello world").
%% Example: stomp:send(Conn, "/queue/foobar", [{"priority", "15"}],
%%                     "high priority hello world").
%%
%% @end
send(#stomp_conn{socket = Socket}, Destination, Headers, MessageBody) ->
    Message = ["SEND", "\ndestination: ", Destination,
               concatenate_options(Headers), "\n\n", MessageBody, [0]],
    gen_tcp:send(Socket, Message),
    ok.

%% @doc Retrieve messages destined for this client from the server.
%%
%% Example: stomp:get_messages(Conn).
%%
%% @end
get_messages(#stomp_conn{socket = Socket} = Conn) ->
    {ok, Chars} = gen_tcp:recv(Socket, 0),
    get_messages(append_chars(Conn, Chars), []).

%% @doc On message retrieval execute a function
%%
%% Example: MyFunction = fun([_, _, {_, X}]) ->
%%                          io:fwrite("message ~s ~n", [X])
%%                       end,
%%          stomp:on_message(MyFunction, Conn).
%%
%% @end
on_message(F, Conn) ->
    Messages = get_messages(Conn),
    lists:foreach(F, Messages),
    on_message(F, Conn).

%% @doc Begin a transaction
%%
%% Example: stomp:begin_transaction(Conn,
%%              "MyUniqueTransactionIdBlahBlahBlah1234567890").
%%
%% @end
begin_transaction(#stomp_conn{socket = Socket}, TransactionId) ->
    Message = ["BEGIN", "\ntransaction: ", TransactionId, "\n\n", [0]],
    gen_tcp:send(Socket, Message),
    ok.

%% @doc Commit a transaction
%%
%% Example: stomp:commit_transaction(Conn,
%%              "MyUniqueTransactionIdBlahBlahBlah1234567890").
%%
%% @end
commit_transaction(#stomp_conn{socket = Socket}, TransactionId) ->
    Message = ["COMMIT", "\ntransaction: ", TransactionId,
               "\n\n", [0]],
    gen_tcp:send(Socket, Message),
    ok.

%% @doc Abort a transaction
%%
%% Example: stomp:abort_transaction(Conn,
%%              "MyUniqueTransactionIdBlahBlahBlah1234567890").
%%
%% @end
abort_transaction(#stomp_conn{socket = Socket}, TransactionId) ->
    Message = ["ABORT", "\ntransaction: ", TransactionId,
               "\n\n", [0]],
    gen_tcp:send(Socket, Message),
    ok.

%%
%% Helpers
%%

concatenate_options(Options) ->
    [[<<"\n">>, Name, <<": ">>, Value] || {Name, Value} <- Options].

%%
%% New message parsing
%% (see http://stomp.github.io/stomp-specification-1.2.html)
%%

get_messages(Conn, AccMessages) ->
    case scan_tokens(Conn) of
        {eof, NewConn} ->
            {lists:reverse(AccMessages), NewConn};
        {more, NewConn} ->
            {lists:reverse(AccMessages), NewConn};
        {Tokens, NewConn} ->
            {ok, [Message]} = stomp_parser:parse(Tokens),
            get_messages(NewConn, [Message|AccMessages])
    end.

scan_tokens(#stomp_conn{cont = Cont, chars = Chars} = Conn) ->
    more_tokens(set(Conn, [{#stomp_conn.chars, []},
                           {#stomp_conn.cont, []}]),
                stomp_lexer:tokens(Cont, Chars)).

more_tokens(Conn, {more, Cont}) ->
    {more, set(Conn, [{#stomp_conn.chars, []},
                      {#stomp_conn.cont, Cont}])};
more_tokens(Conn, {done, {ok, Tokens, _}, RestChars}) ->
    {Tokens, set(Conn, [{#stomp_conn.chars, RestChars},
                        {#stomp_conn.cont, []}])};
more_tokens(Conn, {done, {eof, _}, RestChars}) ->
    {eof, set(Conn, [{#stomp_conn.chars, RestChars},
                     {#stomp_conn.cont, []}])};
more_tokens(Conn, {done, ErrorInfo, RestChars}) ->
    error({ErrorInfo, Conn, RestChars}).

append_chars(#stomp_conn{chars = Chars} = Conn, MoreChars) ->
    set_chars(Conn, Chars ++ MoreChars).

set_chars(#stomp_conn{} = Conn, Chars) ->
    Conn#stomp_conn{chars = Chars}.

%% @doc Set `Fields' of the `Record' to `Values',
%%      where `{Field, Value} <- FieldValues' (in list comprehension syntax).
%% @end
set(Record, FieldValues) ->
    F = fun({Field, Value}, Rec) ->
                setelement(Field, Rec, Value)
        end,
    lists:foldl(F, Record, FieldValues).
