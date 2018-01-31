Definitions.

KEYWORDS=true|false|if|then|else|while|do|and|or
SYMBOLS=[{}:=<>();+-/*]
WHITESPACE=[\s\t\r\n]+
VAR=[a-z]+
NUM=[0-9]+

Rules.

{KEYWORDS}   : {token, {list_to_atom(TokenChars), TokenLine}}.
{SYMBOLS}    : {token, {list_to_atom(TokenChars), TokenLine}}.
{VAR}    	 : {token, {var, TokenLine, list_to_atom(TokenChars)}}.
{NUM}        : {token, {int, TokenLine, list_to_integer(TokenChars)}}.
{WHITESPACE} : skip_token.

Erlang code.