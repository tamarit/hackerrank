Nonterminals aexp bexp stmt.
Terminals 'var' 'int' 'true' 'false' 'if' 'then' 'else' 'while' 'do' 'and' 'or' '{' '}' ':' '=' '<' '>' '(' ')' ';' '+' '-' '*' '/'.
Rootsymbol stmt.


% Arithmetic Operators: (*, /) > (+, -) > (>, <)
% Boolean Operators: and > or

Right 50 ';'.
% Left 100 '<'.
Left 200 '>' '<'.
% Left 300 '-'.
Left 400 '+' '-'.
% Left 500 '/'.
Left 600 '*' '/'.
% Left 700 'or'.
Left 800 'and' 'or'.

% arithmetic expressions
% a ::= x | n | a1 opa a2 | ( a )

aexp -> 
	'var':
	'$1'.
aexp -> 
	'int':
	'$1'.
aexp -> 
	aexp '+' aexp:
	{op, op('$2'), '$1', '$3'}.
aexp -> 
	aexp '-' aexp:
	{op, op('$2'), '$1', '$3'}.
aexp -> 
	aexp '*' aexp:
	{op, op('$2'), '$1', '$3'}.
aexp -> 
	aexp '/' aexp:
	{op, op('$2'), '$1', '$3'}.
aexp -> 
	'(' aexp ')':
	'$2'.

% boolean expressions
% b ::= true | false | b1 opb b2 | a1 opr a2 | ( b )

bexp -> 
	'true':
	true.
bexp -> 
	'false':
	false.
bexp -> 
	bexp 'and' bexp:
	{op, op('$2'), '$1', '$3'}.
bexp -> 
	bexp 'or' bexp:
	{op, op('$2'), '$1', '$3'}.
bexp -> 
	aexp '<' aexp:
	{op, op('$2'), '$1', '$3'}.
bexp -> 
	aexp '>' aexp:
	{op, op('$2'), '$1', '$3'}.
bexp -> 
	'(' bexp ')':
	'$2'.

% statements
% S ::= x := a | S1 ; S2 | if b then { S1 } else { S2 } | while b do { S }

stmt -> 
	'var' ':' '=' aexp:
	[{op, op('$3'), '$1', '$4'}].
stmt -> 
	stmt ';' stmt:
	[hd('$1') | '$3'].
stmt ->
	'if' bexp 'then' '{' stmt '}' 'else' '{' stmt '}':
	[{'if', '$2', '$5', '$9'}].
stmt ->
	'while' bexp 'do' '{' stmt '}':
	[{while, '$2', '$5'}].

Erlang code.
op(T) ->
	element(1, T).