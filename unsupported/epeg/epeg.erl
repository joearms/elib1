-module(epeg).
-compile(export_all).
-import(lists, [map/2, reverse/1]).


-import(lists, [flatten/1]).

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
    io:format("Parsing:~p~n",[F]),
    {ok, B} = file:read_file(F),
    L = binary_to_list(B),
    {Time, {Val, []}} = timer:tc(?MODULE, parse_grammar, [L]),
    Rate = Time/length(L),
    io:format("time:~p (ms)"
	      "#chars = ~w rate=~p~n", [Time/1000, length(L), Rate]),
    Val.


%% -------------- begin ----------------
%% DEFN grammar = - (code | def | unexpected)+ endOfFile ;

%% Abstract syntax:
%%   {seq,[{nt,"-"},
%%         {plus,{alt,[{nt,"code"},{nt,"def"},{nt,"unexpected"}]}},
%%         {nt,"endOfFile"}]}
%% 

parse_grammar(S)->
    {Val, S1} =
        (epeg_pc:seq([
              fun 'parse_-'/1,
              epeg_pc:plus(epeg_pc:alt([
                  fun parse_code/1,
                  fun parse_def/1,
                  fun parse_unexpected/1])),
              fun parse_endOfFile/1]))(S),
    {epeg_pc:done(?MODULE,grammar,Val), S1}.

%% =============== end =================

%% -------------- begin ----------------
%% DEFN unexpected = . ;

%% Abstract syntax:
%%   any
%% 

parse_unexpected(S)->
    {Val, S1} =
        (epeg_pc:any())(S),
    {epeg_pc:done(?MODULE,unexpected,Val), S1}.

%% =============== end =================

%% -------------- begin ----------------
%% DEFN endOfFile = !. ;

%% Abstract syntax:
%%   {bang,any}
%% 

parse_endOfFile(S)->
    {Val, S1} =
        (epeg_pc:bang(epeg_pc:any()))(S),
    {epeg_pc:done(?MODULE,endOfFile,Val), S1}.

%% =============== end =================

%% -------------- begin ----------------
%% DEFN code = "%{" (!'%}' .)* '%}' ;

%% Abstract syntax:
%%   {seq,[{lit,"%{"},
%%         {star,{seq,[{bang,{litthenspace,"%}"}},any]}},
%%         {litthenspace,"%}"}]}
%% 

parse_code(S)->
    {Val, S1} =
        (epeg_pc:seq([
              fun("%{" ++ V1) -> {'%{', V1} end ,
              epeg_pc:star(epeg_pc:seq([
                  epeg_pc:bang(fun("%}" ++ V1) -> {'%}', epeg_pc:drop_space(?MODULE,V1)} end ),
                  epeg_pc:any()])),
              fun("%}" ++ V1) -> {'%}', epeg_pc:drop_space(?MODULE,V1)} end ]))(S),
    {epeg_pc:done(?MODULE,code,Val), S1}.

%% =============== end =================

%% -------------- begin ----------------
%% DEFN def = ident '=' expr ';' - ;

%% Abstract syntax:
%%   {seq,[{nt,"ident"},
%%         {litthenspace,"="},
%%         {nt,"expr"},
%%         {litthenspace,";"},
%%         {nt,"-"}]}
%% 

parse_def(S)->
    {Val, S1} =
        (epeg_pc:seq([
              fun parse_ident/1,
              fun("=" ++ V1) -> {'=', epeg_pc:drop_space(?MODULE,V1)} end ,
              fun parse_expr/1,
              fun(";" ++ V1) -> {';', epeg_pc:drop_space(?MODULE,V1)} end ,
              fun 'parse_-'/1]))(S),
    {epeg_pc:done(?MODULE,def,Val), S1}.

%% =============== end =================

%% -------------- begin ----------------
%% DEFN expr = seq ('|' seq)* - ;

%% Abstract syntax:
%%   {seq,[{nt,"seq"},{star,{seq,[{litthenspace,"|"},{nt,"seq"}]}},{nt,"-"}]}
%% 

parse_expr(S)->
    {Val, S1} =
        (epeg_pc:seq([
              fun parse_seq/1,
              epeg_pc:star(epeg_pc:seq([
                  fun("|" ++ V1) -> {'|', epeg_pc:drop_space(?MODULE,V1)} end ,
                  fun parse_seq/1])),
              fun 'parse_-'/1]))(S),
    {epeg_pc:done(?MODULE,expr,Val), S1}.

%% =============== end =================

%% -------------- begin ----------------
%% DEFN seq = prefix prefix* ;

%% Abstract syntax:
%%   {seq,[{nt,"prefix"},{star,{nt,"prefix"}}]}
%% 

parse_seq(S)->
    {Val, S1} =
        (epeg_pc:seq([
              fun parse_prefix/1,
              epeg_pc:star(fun parse_prefix/1)]))(S),
    {epeg_pc:done(?MODULE,seq,Val), S1}.

%% =============== end =================

%% -------------- begin ----------------
%% DEFN prefix = '&' action | '&' suffix | '!' suffix | suffix ;

%% Abstract syntax:
%%   {alt,[{seq,[{litthenspace,"&"},{nt,"action"}]},
%%         {seq,[{litthenspace,"&"},{nt,"suffix"}]},
%%         {seq,[{litthenspace,"!"},{nt,"suffix"}]},
%%         {nt,"suffix"}]}
%% 

parse_prefix(S)->
    {Val, S1} =
        (epeg_pc:alt([
              epeg_pc:seq([
                  fun("&" ++ V1) -> {'&', epeg_pc:drop_space(?MODULE,V1)} end ,
                  fun parse_action/1]),
              epeg_pc:seq([
                  fun("&" ++ V1) -> {'&', epeg_pc:drop_space(?MODULE,V1)} end ,
                  fun parse_suffix/1]),
              epeg_pc:seq([
                  fun("!" ++ V1) -> {'!', epeg_pc:drop_space(?MODULE,V1)} end ,
                  fun parse_suffix/1]),
              fun parse_suffix/1]))(S),
    {epeg_pc:done(?MODULE,prefix,Val), S1}.

%% =============== end =================

%% -------------- begin ----------------
%% DEFN suffix = primary ('?' | '*' | '+')? ;

%% Abstract syntax:
%%   {seq,[{nt,"primary"},
%%         {question,{alt,[{litthenspace,"?"},
%%                         {litthenspace,"*"},
%%                         {litthenspace,"+"}]}}]}
%% 

parse_suffix(S)->
    {Val, S1} =
        (epeg_pc:seq([
              fun parse_primary/1,
              epeg_pc:question(epeg_pc:alt([
                  fun("?" ++ V1) -> {'?', epeg_pc:drop_space(?MODULE,V1)} end ,
                  fun("*" ++ V1) -> {'*', epeg_pc:drop_space(?MODULE,V1)} end ,
                  fun("+" ++ V1) -> {'+', epeg_pc:drop_space(?MODULE,V1)} end ]))]))(S),
    {epeg_pc:done(?MODULE,suffix,Val), S1}.

%% =============== end =================

%% -------------- begin ----------------
%% DEFN primary = ident ':' ident !'=' | ident !'=' | '(' expr ')' | literal | class | '.' ;

%% Abstract syntax:
%%   {alt,[{seq,[{nt,"ident"},
%%               {litthenspace,":"},
%%               {nt,"ident"},
%%               {bang,{litthenspace,"="}}]},
%%         {seq,[{nt,"ident"},{bang,{litthenspace,"="}}]},
%%         {seq,[{litthenspace,"("},{nt,"expr"},{litthenspace,")"}]},
%%         {nt,"literal"},
%%         {nt,"class"},
%%         {litthenspace,"."}]}
%% 

parse_primary(S)->
    {Val, S1} =
        (epeg_pc:alt([
              epeg_pc:seq([
                  fun parse_ident/1,
                  fun(":" ++ V1) -> {':', epeg_pc:drop_space(?MODULE,V1)} end ,
                  fun parse_ident/1,
                  epeg_pc:bang(fun("=" ++ V1) -> {'=', epeg_pc:drop_space(?MODULE,V1)} end )]),
              epeg_pc:seq([
                  fun parse_ident/1,
                  epeg_pc:bang(fun("=" ++ V1) -> {'=', epeg_pc:drop_space(?MODULE,V1)} end )]),
              epeg_pc:seq([
                  fun("(" ++ V1) -> {'(', epeg_pc:drop_space(?MODULE,V1)} end ,
                  fun parse_expr/1,
                  fun(")" ++ V1) -> {')', epeg_pc:drop_space(?MODULE,V1)} end ]),
              fun parse_literal/1,
              fun parse_class/1,
              fun("." ++ V1) -> {'.', epeg_pc:drop_space(?MODULE,V1)} end ]))(S),
    {epeg_pc:done(?MODULE,primary,Val), S1}.

%% =============== end =================

%% -------------- begin ----------------
%% DEFN int = [0-9]+ ;

%% Abstract syntax:
%%   {plus,{class,[{48,57}]}}
%% 

parse_int(S)->
    {Val, S1} =
        (epeg_pc:plus(epeg_pc:cc([{48,57}])))(S),
    {epeg_pc:done(?MODULE,int,Val), S1}.

%% =============== end =================

%% -------------- begin ----------------
%% DEFN ident = "-" - | [a-z] [a-zA-Z_]* - ;

%% Abstract syntax:
%%   {alt,[{seq,[{lit,"-"},{nt,"-"}]},
%%         {seq,[{class,[{97,122}]},
%%               {star,{class,[{97,122},{65,90},95]}},
%%               {nt,"-"}]}]}
%% 

parse_ident(S)->
    {Val, S1} =
        (epeg_pc:alt([
              epeg_pc:seq([
                  fun("-" ++ V1) -> {'-', V1} end ,
                  fun 'parse_-'/1]),
              epeg_pc:seq([
                  epeg_pc:cc([{97,122}]),
                  epeg_pc:star(epeg_pc:cc([{97,122},{65,90},95])),
                  fun 'parse_-'/1])]))(S),
    {epeg_pc:done(?MODULE,ident,Val), S1}.

%% =============== end =================

%% -------------- begin ----------------
%% DEFN literal = lit - ;

%% Abstract syntax:
%%   {seq,[{nt,"lit"},{nt,"-"}]}
%% 

parse_literal(S)->
    {Val, S1} =
        (epeg_pc:seq([
              fun parse_lit/1,
              fun 'parse_-'/1]))(S),
    {epeg_pc:done(?MODULE,literal,Val), S1}.

%% =============== end =================

%% -------------- begin ----------------
%% DEFN lit = ["] (!["] char)* ["] | ['] (!['] char)* ['] ;

%% Abstract syntax:
%%   {alt,[{seq,[{class,"\""},
%%               {star,{seq,[{bang,{class,"\""}},{nt,"char"}]}},
%%               {class,"\""}]},
%%         {seq,[{class,"'"},
%%               {star,{seq,[{bang,{class,"'"}},{nt,"char"}]}},
%%               {class,"'"}]}]}
%% 

parse_lit(S)->
    {Val, S1} =
        (epeg_pc:alt([
              epeg_pc:seq([
                  epeg_pc:cc("\""),
                  epeg_pc:star(epeg_pc:seq([
                      epeg_pc:bang(epeg_pc:cc("\"")),
                      fun parse_char/1])),
                  epeg_pc:cc("\"")]),
              epeg_pc:seq([
                  epeg_pc:cc("'"),
                  epeg_pc:star(epeg_pc:seq([
                      epeg_pc:bang(epeg_pc:cc("'")),
                      fun parse_char/1])),
                  epeg_pc:cc("'")])]))(S),
    {epeg_pc:done(?MODULE,lit,Val), S1}.

%% =============== end =================

%% -------------- begin ----------------
%% DEFN char = "\" [0-3] [0-7] [0-7] | "\" [0-7] [0-7] | "\" [0-7] | "\" [bdefnrstv'"] | "\^" [a-zA-Z] | "\\" | !"\" . | "\" . | . ;

%% Abstract syntax:
%%   {alt,[{seq,[{lit,"\\"},
%%               {class,[{48,51}]},
%%               {class,[{48,55}]},
%%               {class,[{48,55}]}]},
%%         {seq,[{lit,"\\"},{class,[{48,55}]},{class,[{48,55}]}]},
%%         {seq,[{lit,"\\"},{class,[{48,55}]}]},
%%         {seq,[{lit,"\\"},{class,"bdefnrstv'\""}]},
%%         {seq,[{lit,"\\^"},{class,[{97,122},{65,90}]}]},
%%         {lit,"\\\\"},
%%         {seq,[{bang,{lit,"\\"}},any]},
%%         {seq,[{lit,"\\"},any]},
%%         any]}
%% 

parse_char(S)->
    {Val, S1} =
        (epeg_pc:alt([
              epeg_pc:seq([
                  fun("\\" ++ V1) -> {'\\', V1} end ,
                  epeg_pc:cc([{48,51}]),
                  epeg_pc:cc([{48,55}]),
                  epeg_pc:cc([{48,55}])]),
              epeg_pc:seq([
                  fun("\\" ++ V1) -> {'\\', V1} end ,
                  epeg_pc:cc([{48,55}]),
                  epeg_pc:cc([{48,55}])]),
              epeg_pc:seq([
                  fun("\\" ++ V1) -> {'\\', V1} end ,
                  epeg_pc:cc([{48,55}])]),
              epeg_pc:seq([
                  fun("\\" ++ V1) -> {'\\', V1} end ,
                  epeg_pc:cc("bdefnrstv'\"")]),
              epeg_pc:seq([
                  fun("\\^" ++ V1) -> {'\\^', V1} end ,
                  epeg_pc:cc([{97,122},{65,90}])]),
              fun("\\\\" ++ V1) -> {'\\\\', V1} end ,
              epeg_pc:seq([
                  epeg_pc:bang(fun("\\" ++ V1) -> {'\\', V1} end ),
                  epeg_pc:any()]),
              epeg_pc:seq([
                  fun("\\" ++ V1) -> {'\\', V1} end ,
                  epeg_pc:any()]),
              epeg_pc:any()]))(S),
    {epeg_pc:done(?MODULE,char,Val), S1}.

%% =============== end =================

%% -------------- begin ----------------
%% DEFN class = '[' (!']' range)* ']' - ;

%% Abstract syntax:
%%   {seq,[{litthenspace,"["},
%%         {star,{seq,[{bang,{litthenspace,"]"}},{nt,"range"}]}},
%%         {litthenspace,"]"},
%%         {nt,"-"}]}
%% 

parse_class(S)->
    {Val, S1} =
        (epeg_pc:seq([
              fun("[" ++ V1) -> {'[', epeg_pc:drop_space(?MODULE,V1)} end ,
              epeg_pc:star(epeg_pc:seq([
                  epeg_pc:bang(fun("]" ++ V1) -> {']', epeg_pc:drop_space(?MODULE,V1)} end ),
                  fun parse_range/1])),
              fun("]" ++ V1) -> {']', epeg_pc:drop_space(?MODULE,V1)} end ,
              fun 'parse_-'/1]))(S),
    {epeg_pc:done(?MODULE,class,Val), S1}.

%% =============== end =================

%% -------------- begin ----------------
%% DEFN range = char "-" char | char ;

%% Abstract syntax:
%%   {alt,[{seq,[{nt,"char"},{lit,"-"},{nt,"char"}]},{nt,"char"}]}
%% 

parse_range(S)->
    {Val, S1} =
        (epeg_pc:alt([
              epeg_pc:seq([
                  fun parse_char/1,
                  fun("-" ++ V1) -> {'-', V1} end ,
                  fun parse_char/1]),
              fun parse_char/1]))(S),
    {epeg_pc:done(?MODULE,range,Val), S1}.

%% =============== end =================

%% -------------- begin ----------------
%% DEFN action = '{' braces* '}' - ;

%% Abstract syntax:
%%   {seq,[{litthenspace,"{"},{star,{nt,"braces"}},{litthenspace,"}"},{nt,"-"}]}
%% 

parse_action(S)->
    {Val, S1} =
        (epeg_pc:seq([
              fun("{" ++ V1) -> {'{', epeg_pc:drop_space(?MODULE,V1)} end ,
              epeg_pc:star(fun parse_braces/1),
              fun("}" ++ V1) -> {'}', epeg_pc:drop_space(?MODULE,V1)} end ,
              fun 'parse_-'/1]))(S),
    {epeg_pc:done(?MODULE,action,Val), S1}.

%% =============== end =================

%% -------------- begin ----------------
%% DEFN braces = '{' (!'}' .)* '}' | !'}' . ;

%% Abstract syntax:
%%   {alt,[{seq,[{litthenspace,"{"},
%%               {star,{seq,[{bang,{litthenspace,"}"}},any]}},
%%               {litthenspace,"}"}]},
%%         {seq,[{bang,{litthenspace,"}"}},any]}]}
%% 

parse_braces(S)->
    {Val, S1} =
        (epeg_pc:alt([
              epeg_pc:seq([
                  fun("{" ++ V1) -> {'{', epeg_pc:drop_space(?MODULE,V1)} end ,
                  epeg_pc:star(epeg_pc:seq([
                      epeg_pc:bang(fun("}" ++ V1) -> {'}', epeg_pc:drop_space(?MODULE,V1)} end ),
                      epeg_pc:any()])),
                  fun("}" ++ V1) -> {'}', epeg_pc:drop_space(?MODULE,V1)} end ]),
              epeg_pc:seq([
                  epeg_pc:bang(fun("}" ++ V1) -> {'}', epeg_pc:drop_space(?MODULE,V1)} end ),
                  epeg_pc:any()])]))(S),
    {epeg_pc:done(?MODULE,braces,Val), S1}.

%% =============== end =================

%% -------------- begin ----------------
%% DEFN spaces = (space | comment)* ;

%% Abstract syntax:
%%   {star,{alt,[{nt,"space"},{nt,"comment"}]}}
%% 

parse_spaces(S)->
    {Val, S1} =
        (epeg_pc:star(epeg_pc:alt([
              fun parse_space/1,
              fun parse_comment/1])))(S),
    {epeg_pc:done(?MODULE,spaces,Val), S1}.

%% =============== end =================

%% -------------- begin ----------------
%% DEFN - = (space | comment)* ;

%% Abstract syntax:
%%   {star,{alt,[{nt,"space"},{nt,"comment"}]}}
%% 

'parse_-'(S)->
    {Val, S1} =
        (epeg_pc:star(epeg_pc:alt([
              fun parse_space/1,
              fun parse_comment/1])))(S),
    {epeg_pc:done(?MODULE,'-',Val), S1}.

%% =============== end =================

%% -------------- begin ----------------
%% DEFN space = " " | "	" | endofline ;

%% Abstract syntax:
%%   {alt,[{lit," "},{lit,"\t"},{nt,"endofline"}]}
%% 

parse_space(S)->
    {Val, S1} =
        (epeg_pc:alt([
              fun(" " ++ V1) -> {' ', V1} end ,
              fun("\t" ++ V1) -> {'\t', V1} end ,
              fun parse_endofline/1]))(S),
    {epeg_pc:done(?MODULE,space,Val), S1}.

%% =============== end =================

%% -------------- begin ----------------
%% DEFN comment = "#" (!endofline .)* endofline ;

%% Abstract syntax:
%%   {seq,[{lit,"#"},
%%         {star,{seq,[{bang,{nt,"endofline"}},any]}},
%%         {nt,"endofline"}]}
%% 

parse_comment(S)->
    {Val, S1} =
        (epeg_pc:seq([
              fun("#" ++ V1) -> {'#', V1} end ,
              epeg_pc:star(epeg_pc:seq([
                  epeg_pc:bang(fun parse_endofline/1),
                  epeg_pc:any()])),
              fun parse_endofline/1]))(S),
    {epeg_pc:done(?MODULE,comment,Val), S1}.

%% =============== end =================

%% -------------- begin ----------------
%% DEFN endofline = "
%% " | "
%% " | "" ;

%% Abstract syntax:
%%   {alt,[{lit,"\r\n"},{lit,"\n"},{lit,"\r"}]}
%% 

parse_endofline(S)->
    {Val, S1} =
        (epeg_pc:alt([
              fun("\r\n" ++ V1) -> {'\r\n', V1} end ,
              fun("\n" ++ V1) -> {'\n', V1} end ,
              fun("\r" ++ V1) -> {'\r', V1} end ]))(S),
    {epeg_pc:done(?MODULE,endofline,Val), S1}.

%% =============== end =================



-define(IN(X,Low,Hi),Low =< X, X =< Hi).

final(grammar, [_,G,_]) -> {gram,G};

final(ws, _) -> ws;

final(unexpected, X) ->
    io:format("Unexpected symbol:~p",[[X]]),
    {error, [X]};

final(action, [_,B,_,_]) -> {action, B};

final(class, [_,X,_,_]) -> {class, reduce(X)};

%% char sucks -- fortunately we only have to define it once

final(char, X) ->
    char1(X);
	    
final(spaces, X) ->
    {spaces, flatten(X)};
    %% {spaces, X}.

final('-', _) ->
    spaces;

final(range, [A,_,B]) -> {A,B};
final(range, X) -> X;

final(endofline, _) -> eol;

final(space, X) -> X;

final(comment, X) -> {comment, X};

final(code, [_,L,_]) ->
    R = reduce(L),
    io:format("code (~w bytes) ...~n",[length(R)]),
    {code, R};

final(endOfFile, _) -> endofFile;

final(ident, ['-',spaces]) -> "-";
final(ident, [A,B,spaces]) -> [A|B];

final(line, L) -> reduce(L);

final(def, [Id,'=',Expr,';',_]) ->
    io:format("~p ",[Id]),
    {defn, Id, Expr};

final(expr, [Seq, More, _]) ->
   L = reduce(More),
   case [Seq|L] of
     [Z] -> Z;
     Many -> {alt, Many} 
   end;

final(seq, [A,B]) ->
    case [A|B] of
       [Z] -> Z;
       L -> {seq, L}
    end;

final(prefix, ['&', A]) -> {land, A};
final(prefix, ['!', A]) -> {bang, A};
final(prefix, X) -> X;

final(suffix, [X, []]) -> X;
final(suffix, [X, ['?']]) -> {question, X};
final(suffix, [X, ['+']]) -> {plus, X};
final(suffix, [X, ['*']]) -> {star, X};

final(primary, [Id, bang]) -> {nt, Id};    
final(primary, [Mod, ':', Id, bang]) -> {nt, Mod,Id};    
final(primary, ['(',E,')']) -> E;
final(primary, '.') -> any;
final(primary, X) -> X;

final(literal, [X,_]) ->  X;

final(lit, [$',L,$']) -> {litthenspace, reduce(L)};
final(lit, [$",L,$"]) -> {lit, reduce(L)};

final(int, X) -> list_to_integer(X);

final(erlerror, S) -> {badToken, [S]};

final(dws, _) ->  dot;
final(Tag, S) ->  {Tag, S}.
	   
reduce(L) -> [X || [_,X] <- L].

char1([bang,X]) ->
    X;
char1(['\\', X]) when ?IN(X, $0, $7) ->
    X - $0;
char1(['\\', X, Y]) when ?IN(X, $0, $7), ?IN(Y, $0, $7) ->
    (X-$0) * 8 + (Y - $0);
char1(['\\', X, Y, Z]) when ?IN(X, $0, $3), ?IN(Y, $0, $7), ?IN(Z, $0, $7) ->
    (X-$0) * 64 + (Y-$0) * 8 + (Z - $0);
char1(['\\^', X]) when ?IN(X, $a, $z) ->
    X - $a + 1;
char1(['\\^', X]) when ?IN(X, $A, $Z) ->
    X - $A + 1;
char1('\\\\') ->
    $\\;
char1('\\') ->
    $\\;
char1(['\\', C]) ->
    case C of
	$b -> $\b;
	$d -> $\d;
	$e -> $\e;
	$f -> $\f;
	$n -> $\n;
	$r -> $\r;
	$s -> $\s;
	$t -> $\t;
	$v -> $\v;
	$' -> $';
	$" -> $";
	C -> C
    end;
char1(X) ->
    io:format("ooo char=~p~n",[X]),
    {char, X}.

test() ->
    Mods =  [epeg,epeg_compiler,epeg_pc],
    cover:start(),
    [cover:compile(I) || I <- Mods],
    io:format("Testing...~n"),
    compare_parses("./peglib.peg"),
    compare_parses("./pegtest.peg"),
    compare_parses("./epeg.peg"),
    io:format("all tests work -- PEG works - hooray~n"),
    io:format("Analysing...~n"),
    [{ok,_} = cover:analyse_to_file(I) || I <- Mods],
    io:format("Done...~n"),
    init:stop().

compare_parses(F) ->
    io:format("Testing epeg on:~s~n",[F]),
    %% parse ourself
    Val1 = peg_compile("mod", F),
    io:format("Val1=~p~n",[Val1]),
    Val2 = epeg_bootstrap:peg_compile("mod", F),
    %% io:format("Val2=~p~n",[Val2]),
    case Val1 of
	Val2 ->
	    io:format("YES EPEG Works~n"),
	    yes;
	_ ->
	    io:format("EPEG failed - this is really really bad~n"),
	    dump("debug.tmp", {epeg,Val1,bootstrap,Val2}),
	    init:stop()
    end.

dump(File, Term) ->
    {ok, S} = file:open(File, [write]),
    io:format(S, "~p.~n", [Term]),
    file:close(S).
