%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(epeg_bootstrap).

-export([batch/1, parse_peg_file/1, peg2erl/1, peg_compile/2]).

-export([egrammar/1]).

-import(lists, [map/2, reverse/1]).

-import(epeg_pc,
	[alt/1, any/0, bang/1, block/1, cc/1, char/1, 
	 do/2, lit/1, plus/1, question/1, star/1, seq/1]).

%% peg_to_erl(InFile) -> OutFile
%%    example: peg_to_erl("foo.peg") compiles foo.peg creating
%%    foo.erl

batch([X]) ->
    In = atom_to_list(X),
    peg2erl(In),
    init:stop().    

peg2erl(In) ->
    Mod = filename:rootname(In),
    io:format("compiling:~p~n",[In]),
    Code = peg_compile(Mod, In),
    io:format("* creating * ~s.erl~n",[Mod]),
    file:write_file(Mod ++ ".erl", Code).

peg_compile(Mod, In) ->
    Parse = parse_peg_file(In),
    epeg_compiler:compile(Mod, Parse).

parse_peg_file(F) ->
    io:format("bootstrap parse peg:~p~n",[F]),
    {ok, B} = file:read_file(F),
    L = binary_to_list(B),
    {Time, {Val, []}} = timer:tc(?MODULE, egrammar, [L]),
    Rate = Time/length(L),
    io:format("file:~p time:~p (ms)"
	      " #chars = ~w rate=~p~n", [F, Time/1000, length(L), Rate]),
    Val.

%% grammar= - ( code | definition | skip )+ end-of-file

egrammar(S) ->
    {[_,D,_],S1} = (seq([fun eSpaces/1, 
			 plus(alt([fun ecode/1, 
				   fun edefinition/1])),
			 fun eEndOfFile/1]))(S),
    {{gram,D}, S1}.

%% eccode = '%{' < ( !'%}' . )* > RPERCENT

ecode(S) ->
    {[_,B,_],S1} = (seq([lit("%{"), 
			 block(star(seq([bang(lit("%}")), 
					 any()]))), 
			 fun eRPERCENT/1]))(S),
    io:format("decl~n"),
    {{code,B}, S1}.

%% RPERCENT = '%}' -
eRPERCENT(S)  -> (seq([lit("%}"), fun eSpaces/1]))(S).

%% definition=	ident EQUAL expression { ... } 	SEMICOLON?		

edefinition(S) ->
    {[Id,_,E,_],S1} = (seq([fun eident/1, 
			    fun eEQUAL/1, 
			    fun eexpression/1, 
			    question(fun eSEMICOLON/1)]))(S),
    io:format("~p ",[Id]),
    {{defn,Id,E}, S1}.
     
%% expression=	sequence  (BAR sequence { ... } )*
eexpression(S) ->
    (do(seq([fun esequence/1, 
	     star(
	       do(seq([fun eBAR/1, 
		       fun esequence/1]),
		  fun([_,Seq]) -> Seq end))]),
	fun([U,B]) -> 
		wrapalt([U|B])
	end))(S).

wrapalt([X]) -> X;
wrapalt(L) -> {alt,L}.

%% sequence= prefix (prefix { ... })*
esequence(S) ->
    (do(seq([fun eprefix/1,star(fun eprefix/1)]),
		  fun([A,[]]) -> A;
		     ([A,B]) -> wrapseq([A] ++ B)
		  end))(S).

wrapseq([X]) -> X;
wrapseq(L) -> {seq,L}.

%% prefix=	AND action  {land,P(A)}
%% |		AND suffix  {land,P(A)}
%% |		NOT suffix  {bang, P(A)}
%% |		    suffix  P(A)
eprefix(S) ->
    (alt([do(seq([fun eAND/1, fun eaction/1]),fun([_,A]) -> {land,A} end),
	  do(seq([fun eAND/1, fun esuffix/1]),fun([_,A]) -> {land,A} end),
	  do(seq([fun eNOT/1, fun esuffix/1]),fun([_,A]) -> {bang,A} end),
	  fun esuffix/1]))(S).

%% suffix = primary (QUESTION | STAR | PLUS)?
esuffix(S) ->
    {[X,Y],S1} = (seq([fun eprimary/1, 
		       question(
			 alt([fun eQUESTION/1, 
			      fun eSTAR/1, 
			      fun ePLUS/1]))]))(S),
    case Y of
	[] -> {X, S1};
	[Op] -> {{Op, X},S1}
    end.
	    

%% primary=	ident COLON ident !EQUAL
%% |		ident !EQUAL			
%% |		OPEN expression CLOSE
%% |		literal
%% |		class
%% |		DOT
%% |		BEGIN
%% |		END

eprimary(S) ->
    (alt([
	  do(seq([fun eident/1, 
		  fun eCOLON/1, 
		  fun eident/1, 
		  bang(fun eEQUAL/1)]),
	     fun([Mod,_,Term,_]) ->
		     {nt,Mod,Term}
	     end),
	  do(seq([fun eident/1, bang(fun eEQUAL/1)]),
	     fun([Id,_]) -> {nt,Id} end),
	  do(seq([fun eOPEN/1, fun eexpression/1, fun eCLOSE/1]),
	     fun([_,E,_]) -> E end),
	  fun elit1/1,
	  fun elit2/1,
	  fun eclass/1,
	  fun eDOT/1,
	  fun eBEGIN/1,
	  fun eEND/1]))(S).

%% # Lexical syntax

%% ident = < [-a-zA-Z_][-a-zA-Z_0-9]* > -

eident(S) ->
    (do(seq([block(seq([cc([{$a,$z},{$A,$Z},$-,$_]),
			star(cc([{$a,$z},{$A,$Z},{$0,$9},$-,$_]))])),
	     fun eSpaces/1]),
	fun([X,_]) -> X end))(S).

%% literal  =  ["]  ( !["] char )*  ["] -
%% ' = 39 "=34

elit1(S) ->
    (do(seq([char(39),
	     star(do(seq([bang(char(39)),fun echar/1]),
		     fun([_,X]) -> X end)),
	     char(39), 
	     fun eSpaces/1]),
	fun([_,B,_,_]) -> 
		{litthenspace, B} 
	end))(S).

%% 34 = "" this is the double quoted literal

elit2(S) ->
    (do(seq([char(34),
	     star(do(seq([bang(char(34)),fun echar/1]),
		     fun([_,X]) -> X end)),
	     char(34), 
	     fun eSpaces/1]),
	fun([_,B,_,_]) ->
		%% io:format("read lit=|~s|~n",[B]),
		{lit, B} 
	end))(S).

%% class = '['  ( !']' range )* ']' -
%% 91 = [ 93 = ]

eclass(S) ->
    {[_,B,_,_],S1} = (seq([char(91),
			   star(
			     do(seq([bang(char(93)), fun erange/1]),
				fun([_,X]) -> X end)),
			   char(93),
			   fun eSpaces/1]))(S),
    {{class, B}, S1}.

%% range = char '-' char | char

erange(S) ->
    {A, S1} = (alt([seq([fun echar/1, char($-), fun echar/1]),
		    fun echar/1]))(S),
    R = case A of
	    [C1,_,C2] -> {C1,C2};
	    C -> C
    end,
    {R, S1}.

%% char = '\\' [enrtv'"\[\]\\]
%%      | '\\' [0-3][0-7][0-7]
%%      | '\\' [0-7][0-7]?
%%      | !'\\' .

echar(S) ->
    (alt([	      
	  do(seq([char($\\),cc("enrtv'\"[]\\")]),
	     fun([_,C]) ->
		     case C of
			 $e -> $\e;
			 $n -> $\n;
			 $r -> $\r;
			 $s -> $\s;
			 $t -> $\t;
			 $v -> $\v;
			 X -> X
		     end
	     end),
	     do(seq([char($\\),cc([{$0,$3},{$0,$7},{$0,$7}])]),
		fun([_,P,Q,R]) ->
			R + Q*8 + P*8*8
		end),
	     do(seq([char($\\),cc([{$0,$3}]),question(cc([{$0,$7}]))]),
		fun([_,A,[]]) -> A;
		   ([_,A,[B]]) -> A*8 + B
		end),
	     do(seq([bang(char($\\)), any()]),
		fun([_,A]) -> A end)
		   ]))(S).

%% action = '{' < braces* > '}' -

eaction(S) ->
    {[_,B,_,_],S1} = (seq([char(${),
				block(star(fun ebraces/1)),
				char($}), fun eSpaces/1]))(S),
    {{action,B}, S1}.

%% braces = '{' (!'}' .)* '}'
%%        | !'}' .

%% 123 = ${   125 = $} written like this because emacs electic mode does
%% does indent $(bracket) correctly :-)

ebraces(S) ->
    (alt([seq([char(123),star(seq([bang(char(125)),any()])),char(125)]),
	  seq([bang(char(125)),any()])]))(S).

%% EQUAL = '=' -
eEQUAL(S) -> (seq([char($=), fun eSpaces/1]))(S).

%% COLON = ':' -
eCOLON(S) ->  (seq([char($:), fun eSpaces/1]))(S).

%% SEMICOLON = ';' -
eSEMICOLON(S) ->  (seq([char($;), fun eSpaces/1]))(S).

%% BAR = '|' -
eBAR(S) ->  (seq([char($|), fun eSpaces/1]))(S).

%% AND =	'&' -
eAND(S) ->  (seq([char($&), fun eSpaces/1]))(S).

%% NOT = '!' -
eNOT(S) -> (seq([char($!), fun eSpaces/1]))(S). 

%% QUESTION = '?' -
eQUESTION(S) ->  
    {_, S1} = (seq([char($?), fun eSpaces/1]))(S),
    {question, S1}.

%% STAR = '*' -
eSTAR(S) ->  
    {_, S1} = (seq([char($*), fun eSpaces/1]))(S),
    {star, S1}.

%% PLUS = '+' -
ePLUS(S) -> 
    {_, S1} = (seq([char($+), fun eSpaces/1]))(S),
    {plus, S1}.

%% OPEN = '(' -
eOPEN(S) -> (seq([char($(), fun eSpaces/1]))(S).

%% CLOSE = ')' -
eCLOSE(S) -> (seq([char($)), fun eSpaces/1]))(S).

%% DOT =	'.' -
eDOT(S) -> 
    {_, S1} = (seq([char($.), fun eSpaces/1]))(S),
    {any,S1}.

%% BEGIN = '<' -
eBEGIN(S) ->  (seq([char($<), fun eSpaces/1]))(S).

%% END = '>' -
eEND(S) ->  (seq([char($>), fun eSpaces/1]))(S).

%% -= (space | comment)*
eSpaces(S) -> (star(alt([fun espace/1,fun ecomment/1])))(S).

%% space = ' ' | '\t' | end-of-line
espace(S) ->
    (alt([char($\s),char($\t), fun eEndOfLine/1]))(S).

%% comment = '#' (!end-of-line .)* end-of-line
ecomment(S) ->
    (seq([char($#),star(seq([bang(fun eEndOfLine/1), any()])), fun eEndOfLine/1]))(S).

%% end-of-line = '\r\n' | '\n' | '\r'
eEndOfLine(S) ->
    (alt([alit("\r\n"),char($\n),char($\r)]))(S).

%% endOfFile = ! .

eEndOfFile(S) ->
    (bang(any()))(S).

alit(X) ->    
    fun(S1) ->
	    {Val, S2} = (lit(X))(S1),
	    {{lit,Val}, S2}
    end.

