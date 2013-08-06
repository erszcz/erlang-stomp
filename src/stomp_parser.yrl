Nonterminals
frame_stream frame headers header.

Terminals
':' eol command octets header_chars null.

Rootsymbol frame_stream.

frame_stream -> frame frame_stream : strip_eol_frame('$1', '$2').
frame_stream -> frame : ['$1'].

frame -> command eol
         headers eol
         octets
         null : [{type, unwrap('$1')},
                 {headers, '$3'},
                 {body, unwrap('$5')}].
frame -> command eol
         headers eol
         null : [{type, unwrap('$1')},
                 {headers, '$3'},
                 {body, []}].
frame -> eol : [].

headers -> header headers : ['$1'|'$2'].
headers -> header : ['$1'].

header -> header_chars ':' header_chars eol : {unwrap('$1'), unwrap('$3')}.
header -> header_chars ':' eol : {unwrap('$1'), unwrap("")}.

Erlang code.

strip_eol_frame([], Frames) ->
    Frames;
strip_eol_frame(Frame, Frames) ->
    [Frame | Frames].

unwrap({command, _, Command}) ->
    Command;
unwrap({header_chars, _, Chars}) ->
    Chars;
unwrap({octets, _, Octets}) ->
    Octets.
