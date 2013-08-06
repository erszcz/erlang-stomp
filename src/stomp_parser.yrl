Nonterminals
frame_stream frame headers header.

Terminals
':' eol command octets null.

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

header -> octets ':' octets eol : {unwrap('$1'), unwrap('$3')}.
header -> octets ':' eol : {unwrap('$1'), unwrap("")}.

Erlang code.

strip_eol_frame([], Frames) ->
    Frames;
strip_eol_frame(Frame, Frames) ->
    [Frame | Frames].

unwrap({command, _, Command}) ->
    Command;
unwrap({octets, _, Octets}) ->
    Octets.
