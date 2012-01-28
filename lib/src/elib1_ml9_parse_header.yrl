Nonterminals
form kv args term  record tuple_args list_args.

Terminals
int atom string
',' '{' '}' '[' ']' '='.

Rootsymbol form.

form -> record : '$1'.

record -> '{' args '}' : '$2'.
record -> '{' '}': [].

args -> kv          : ['$1'].
args -> kv ',' args : ['$1'|'$3'].

kv -> term '=' term :  {'$1', '$3'}.

term -> atom: unwrap('$1').
term -> int : unwrap('$1').
term -> string : unwrap('$1').
term -> '{' tuple_args '}': list_to_tuple('$2').
term -> '{' '}' : {}.
term -> '[' ']' : [].
term -> '[' list_args ']' : '$2'.


tuple_args -> term: ['$1'].
tuple_args -> term ',' tuple_args: ['$1'|'$3'].

list_args -> term: ['$1'].
list_args -> term ',' list_args: ['$1'|'$3'].

Erlang code.

unwrap({X,_}) -> X;
unwrap({_,_,V}) -> V.
