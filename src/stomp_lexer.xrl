Definitions.

NULL        = \000
CR          = \r
LF          = \n
OCTET       = [^\000{CR}{LF}\:]

Rules.

CONNECTED   : {token, {command, TokenLine, TokenChars}}.
MESSAGE     : {token, {command, TokenLine, TokenChars}}.
RECEIPT     : {token, {command, TokenLine, TokenChars}}.
ERROR       : {token, {command, TokenLine, TokenChars}}.

\:          : {token, {':', TokenLine}}.
{NULL}      : {end_token, {null, TokenLine}}.
{CR}        : skip_token.
{LF}        : {token, {eol, TokenLine}}.
{OCTET}+    : {token, {octets, TokenLine, TokenChars}}.

Erlang code.

