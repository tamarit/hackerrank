Nonterminals uminus uplus exp term factor.
Terminals 'int' '(' ')' '+' '-' '*' '/'.
Rootsymbol exp.


Right 100 '+' '-'.
Right 200 '*' '/'.
Unary 300 uminus uplus.   

uminus -> '-' factor: {uop, op('$1'), '$2'}. 

uplus -> '+' factor: {uop, op('$1'), '$2'}. 

% Expression ::= Term [+-] Expression | Term

exp -> 
	term '+' exp:
	{bop, op('$2'), '$1', '$3'}.
exp -> 
	term '-' exp:
	{bop, op('$2'), '$1', '$3'}.
exp ->
	term:
	'$1'.

% Factor     ::= Number | [+-] Factor | '(' Expression ')'

factor -> 
	'int':
	'$1'.
factor -> 
	uminus:
	'$1'.
factor -> 
	uplus:
	'$1'.
factor -> 
	'(' exp ')':
	'$2'.

% Term       ::= Factor [*/] Term | Factor

term -> 
	factor '*' term:
	{bop, op('$2'), '$1', '$3'}.
term -> 
	factor '/' term:
	{bop, op('$2'), '$1', '$3'}.
term -> 
	factor:
	'$1'.


Erlang code.
op(T) ->
	element(1, T).