Definitions.

SYMBOLS=[()+-/*]
WHITESPACE=[\s\t\r\n]+
NUM=[0-9]+

Rules.

{SYMBOLS}    : {token, {list_to_atom(TokenChars), TokenLine}}.
{NUM}        : {token, {int, TokenLine, list_to_integer(TokenChars)}}.
{WHITESPACE} : skip_token.

Erlang code.