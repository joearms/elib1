%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(epeg_bootstrap).

-compile(export_all).
-import(lists, [map/2, reverse/1]).
-import(epeg_pc,
	[alt/1, any/0, bang/1, block/1, cc/1, char/1,
	 do/2, lit/1, plus/1, question/1, star/1, seq/1]).

%% leg parse - no (sensible parse tree)

%% tests on the .leg files
%%   Tuples are {File, microseconds, #size, microseconds/byte}
%% > leg:test1().

%% [{"basic.leg",   31127, 9250, 3.36508},
%%  {"calc.leg",    5610,  800,  7.01250},
%%  {"leg.leg",     17722, 6712, 2.64035},
%%  {"username.leg",517,   162,  3.19136},
%%  {"wc.leg",      1546,  425,  3.63765}]

%% Typically 3 micro seconds/byte

test0() ->
    X = (catch (epeg_pc:bang(epeg_pc:lit("%{")))("%{")),
    Z = (catch (bang(lit("%{")))("%{")),
    Y = (catch (ed1())("%{")),
    {X,Z,Y}.

ed1() -> (bang(lit("%{"))).


test() ->
    {{lit,"123"},"abc"} = run(alit("123"), "123abc"),
    fail = run(alit("123"),"abc"),
    {_,"abc"} = ecomment("# 1212313\nabc"),
    {'EXIT', _} = (catch ecomment("12356416")),
    io:format("yes~n"),
    {"abc", "("} = eident("abc("),
    io:format("yes~n"),
    {'EXIT', _} = (catch eident("(a")),
    {_,"aa"} = eSTAR("*     aa"),
    {'EXIT', _} = (catch eSTAR("abc")),
    io:format("yes0~n"),
    {_,"abc"} = esuffix("\"123\" ?  abc"),
    io:format("yesb~n"),
    {_,"abc"} = eprefix("! \"123\" *  abc"),
    io:format("yes1~n"),
    {_, "abc"} = eprefix("& {123 xyz}   abc"),
    {{class,_},"123"} = eclass("[a-zA-Z0-9]  123"),
    {{class,[10,1,{97,98}]},"123"} = eclass("[\n\1a-b]123"),
    {{class,[10,19,{97,98}]},"123"} = eclass("[\n\23a-b]123"),
    {{class,[10,12,50,51,{97,98}]},"123"} = eclass("[\n\f23a-b]123"),
    testsWorked.

test1([X]) ->
    io:format("X=~p~n",[X]),
    F = atom_to_list(X),
    test(),
    peg2erl(F),
    init:stop().

parse_peg_file(F) ->
    io:format("bootstrap parse peg:~p~n",[F]),
    {ok, B} = file:read_file(F),
    L = binary_to_list(B),
    {Time, {Val, []}} = timer:tc(?MODULE, egrammar, [L]),
    Rate = Time/length(L),
    io:format("file:~p time:~p (ms)"
	      " #chars = ~w rate=~p~n", [F, Time/1000, length(L), Rate]),
    Val.

peg2erl([X]) ->
    In = atom_to_list(X),
    F = filename:rootname(In),
    io:format("compiling:~p~n",[In]),
    Parse = parse_peg_file(In),
    Mod = F,
    C = epeg_compiler:compile(Mod, Parse),
    io:format("* creating * ~s.erl",[Mod]),
    file:write_file(Mod ++ ".erl", C),
    init:stop().

test_file(F) ->
    {ok, B} = file:read_file(F),
    L = binary_to_list(B),
    {Time, {Parse,[]}} = timer:tc(?MODULE, lines, [L]),
    lib_misc:dump(F, Parse),
    %% C = compile(Parse, 1),
    %%     file:write_file(F ++ ".erl", C),
    O = {F, Time, size(B), Time/size(B)},
    io:format("~p~n",[O]).

run(F, Str) ->
    case (catch F(Str)) of
	{'EXIT', _} -> fail;
	Val -> Val
    end.

%% # Hierarchical syntax
%% grammar=	- ( code | definition )+ endOfFile

%% lines = line (';' line)* endOfFile;

elines(S) ->
    {[A,B,_],S1} = (seq([fun eline/1,
			 star(seq([
				   char($;),
				   fun eline/1])),
			 fun eEndOfFile/1]))(S),

    B1 = [Line || [_,Line] <- B],
    {[A|B1], S1}.

%% line = < ( !';' .)* >;

eline(S) ->
    (block(star(seq([bang(char(59)), any()]))))(S).



%% skip = (!";" .)* spaces

skip(S) ->
    {B,S1} = (seq([
		   star(seq([bang(char($;)), any()])),
		   fun eSpaces/1]))(S),
    io:format("skipped:~p~n",[B]),
    {skipped,S1}.

%% grammar= - ( code | definition | skip )+ end-of-file
%%
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

ed(S) ->
    (seq([
	  star(seq([bang(lit("%}")),
		    any()]))
	  ]))(S).

%% RPERCENT = '%}' -
eRPERCENT(S)  -> (seq([lit("%}"), fun eSpaces/1]))(S).

%% definition=	identifier EQUAL expression { ... } 	SEMICOLON?
edefinition(S) ->
    {[Id,_,E,_],S1} = (seq([fun eident/1,
			    fun eEQUAL/1,
			    fun eexpression/1,
			    question(fun eSEMICOLON/1)]))(S),
    io:format("~p ",[Id]),
    {{defn,Id,E}, S1}.


%% trailer=	'%%' < .* >				{ makeTrailer(yytext); }
etrailer(S) ->
    (do(seq([alit("%%"),block(star(any()))]),
	fun([_,X]) -> X end))(S).

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


%% primary=	identifier COLON identifier !EQUAL
%% |		identifier !EQUAL
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
	     fun([Var,_,Expr,_]) ->
		     {var,Var,Expr}
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

%% identifier =	< [-a-zA-Z_][-a-zA-Z_0-9]* > -

eident(S) ->
    (do(seq([block(seq([cc([{$a,$z},{$A,$Z},$-,$_]),
			star(cc([{$a,$z},{$A,$Z},{$0,$9},$-,$_]))])),
	     fun eSpaces/1]),
	fun([X,_]) -> X end))(S).

equoted(S) ->
    (do(seq([char(39),
	     star(do(seq([bang(char(39)), any()]),fun([_,X]) -> X end)),
	     char(39),
	     fun eSpaces/1]),
	fun([_,X,_,_]) -> [39|X] ++ [39] end))(S).

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

