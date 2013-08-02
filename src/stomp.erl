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

%% @doc Connect to a STOMP server and return a connection socket
%%
%% Example: Conn = stomp:connect("localhost", 61613, "", "").
%%
%% @end
connect(Host, PortNo, Login, Passcode)  ->
    Message = ["CONNECT", "\nlogin: ", Login, "\npasscode: ", Passcode,
               "\n\n", [0]],
    {ok, Sock} = gen_tcp:connect(Host, PortNo, [{active, false}]),
    gen_tcp:send(Sock, Message),
    {ok, Response} = gen_tcp:recv(Sock, 0),
    [{type, Type}, _, _, _] = get_message(Response), %%UGLY!
    case Type of
        "CONNECTED" ->
            Sock;
        _->
            throw("Error occured during connection attempt.")
    end,
    Sock.

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
%%                           {"activemq.prefetchSize", 1}]).
%%
%% @end
subscribe(Destination, Connection, Options) ->
    Message = ["SUBSCRIBE", "\ndestination: ", Destination,
               concatenate_options(Options), "\n\n", [0]],
    gen_tcp:send(Connection, Message),
    ok.

%% @doc Remove an existing subscription
%%
%% Example: stomp:unsubscribe("/queue/foobar", Conn).
%%
%% @end
unsubscribe(Destination, Connection) ->
    Message = ["UNSUBSCRIBE", "\ndestination: ", Destination, "\n\n", [0]],
    gen_tcp:send(Connection, Message),
    ok.

%% @doc Disconnect gracefully from the existing connection
%%
%% Example: stomp:disconnect(Conn).
%%
%% @end
disconnect(Connection) ->
    Message = ["DISCONNECT", "\n\n", [0]],
    gen_tcp:send(Connection, Message),
    gen_tcp:close(Connection),
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
ack(Connection, MessageId) ->
    AckMessage = ["ACK", "\nmessage-id: ", MessageId, "\n\n", [0]],
    gen_tcp:send(Connection, AckMessage),
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
ack(Connection, MessageId, TransactionId) ->
    AckMessage = ["ACK", "\nmessage-id: ", MessageId,
                  "\ntransaction: ", TransactionId, "\n\n", [0]],
    gen_tcp:send(Connection, AckMessage),
    ok.

%% @doc Send a message to the destination in the messaging system
%%
%% Example: stomp:send(Conn, "/queue/foobar", [], "hello world").
%% Example: stomp:send(Conn, "/queue/foobar", [{"priority", "15"}],
%%                     "high priority hello world").
%%
%% @end
send(Connection, Destination, Headers, MessageBody) ->
    Message = ["SEND", "\ndestination: ", Destination,
               concatenate_options(Headers), "\n\n", MessageBody, [0]],
    gen_tcp:send(Connection, Message),
    ok.

%% @doc Retrieve messages destined for this client from the server.
%%
%% Example: stomp:get_messages(Conn).
%%
%% @end
get_messages(Connection) ->
    get_messages(Connection, []).

get_messages(Connection, Messages) ->
    {ok, Response} = gen_tcp:recv(Connection, 0),
    get_messages(Connection, Messages, Response).

get_messages(_, Messages, []) ->
    Messages;
get_messages(Connection, Messages, Response) ->
    [{type, Type},
     {headers, Headers},
     {body, MessageBody}, TheRest] = get_message(Response),
    [_|T] = TheRest, %% U.G.L.Y.... you ain't got no alibi.
    get_messages(Connection, [Messages, [[{type, Type}, {headers, Headers},
                                          {body, MessageBody}]]], T).

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
    apply_function_to_messages(F, Messages),
    on_message(F, Conn).

%% @doc Begin a transaction
%%
%% Example: stomp:begin_transaction(Conn,
%%              "MyUniqueTransactionIdBlahBlahBlah1234567890").
%%
%% @end
begin_transaction(Connection, TransactionId) ->
    Message = ["BEGIN", "\ntransaction: ", TransactionId, "\n\n", [0]],
    gen_tcp:send(Connection, Message),
    ok.

%% @doc Commit a transaction
%%
%% Example: stomp:commit_transaction(Conn,
%%              "MyUniqueTransactionIdBlahBlahBlah1234567890").
%%
%% @end
commit_transaction(Connection, TransactionId) ->
    Message = ["COMMIT", "\ntransaction: ", TransactionId,
               "\n\n", [0]],
    gen_tcp:send(Connection, Message),
    ok.

%% @doc Abort a transaction
%%
%% Example: stomp:abort_transaction(Conn,
%%              "MyUniqueTransactionIdBlahBlahBlah1234567890").
%%
%% @end
abort_transaction(Connection, TransactionId) ->
    Message = ["ABORT", "\ntransaction: ", TransactionId,
               "\n\n", [0]],
    gen_tcp:send(Connection, Message),
    ok.

%% PRIVATE METHODS...
concatenate_options([]) ->
    [];
concatenate_options([H|T]) ->
    {Name, Value} = H,
    ["\n", Name, ": ", Value, concatenate_options(T)].

apply_function_to_messages(_, []) ->
    ok;
apply_function_to_messages(F, [H|T]) ->
    F(H),
    apply_function_to_messages(F, T).

% MESSAGE PARSING... get's a little ugly in here...
% would help if I truly grokked Erlang, I suspect.
% 7/12/09 - yeah, ugly indeed, i need to make this use the same pattern
% as get_headers_from_raw_src...
% currently scanning header block multiple times and making unnecessary copies
get_message(Message) ->
    [Type,
     {Headers, MessageBody}, TheRest] = get_type(Message), %% Ugly...
    {ParsedHeaders, _} = get_headers_from_raw_src([], Headers),
    [{type, Type}, {headers, ParsedHeaders}, {body, MessageBody}, TheRest].

%% extract message body
get_message_body([H|T]) ->
    get_message_body([H|T], []).

get_message_body([H|T], MessageBody) ->
    case H of
        0 ->
            {MessageBody, T};
        _ ->
            {MyMessageBody, TheRest} = get_message_body(T, MessageBody),
            {[MessageBody, [H], MyMessageBody], TheRest}
    end.

%% extract headers as a blob of chars, after having iterated over...
get_headers(Message) ->
    get_headers(Message, []).

get_headers(Message, Headers) ->
    get_headers(Message, Headers, -1).
get_headers([H|T], Headers, LastChar) ->
    case {H, LastChar} of
        {10, 10} ->
            {MessageBody, TheRest} = get_message_body(T),
            [{Headers, MessageBody}, TheRest];
        {_, _} ->
            get_headers(T, [Headers, [H]], H)
    end.

%% extract type("MESSAGE", "CONNECT", etc.) from message string...
get_type(Message) ->
    get_type(Message, []).

get_type([], Type) ->
    Type;
get_type([H|T], Type) ->
    case H of
        10 ->
            [{Headers, MessageBody}, TheRest] = get_headers(T),
            [Type, {Headers, MessageBody}, TheRest];
        _ ->
            get_type(T, [Type, [H]])
    end.

%% parse header clob into list of tuples...
get_headers_from_raw_src(Headers, []) ->
    {Headers, []};
get_headers_from_raw_src(Headers, RawSrc) ->
    {Header, RestOfList} = get_header(RawSrc),
    get_headers_from_raw_src([Headers, [Header]], RestOfList).

get_header(RawSrc) ->
    {HeaderName, RestOfListAfterHeaderExtraction} =
    get_header_name([], RawSrc),
    {HeaderValue, RestOfListAfterValueExtraction} =
    get_header_value([], RestOfListAfterHeaderExtraction),
    {{HeaderName, HeaderValue}, RestOfListAfterValueExtraction}.

get_header_name(HeaderName, [H|T]) ->
    case H of
        58 ->
            {HeaderName, T};
        _ ->
            get_header_name([HeaderName, [H]], T)
    end.

get_header_value(HeaderValue, [H|T]) ->
    case H of
        10 ->
            {HeaderValue, T};
        _ ->
            get_header_value([HeaderValue, [H]], T)
    end.
