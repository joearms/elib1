%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(epeg_compiler).

%% -compile(export_all).

-import(lists, [map/2, reverse/1]).

%% -import(epeg_pc,
%% 	[alt/1, any/0, bang/1, block/1, cc/1, char/1,
%% 	 do/2, lit/1, plus/1, question/1, star/1, seq/1]).

-export([compile/2]).


compile(Mod, Parse) ->
    {Finaliser, Parse1} = finaliser_module(Parse),
    [header(Mod),compile(Parse1, Finaliser, 1)].

compile({gram, L}, M, _) ->
    [compile(I, M, 1) || I <- L];
compile({code,L}, _, _) ->
    L;
compile({action, S},_, _) ->
    ["epeg_pc:action(\"",S,"\")"];
compile({defn,I,Rhs}, M, _) ->
    N = name1(I),
    [pp(I, Rhs),
     name(I),"(S)->\n",
     indent(4),"{Val, S1} =\n",
     indent(8), "(",compile(Rhs, M, 10),")(S),\n",
     indent(4),"{epeg_pc:done(",M,",", N,",Val), S1}.\n\n",
     "%% =============== end =================\n\n"];
compile({litthenspace,S}, M, _N) ->
    A = list_to_atom(S),
    io_lib:format("fun(~p ++ V1) -> {~p, epeg_pc:drop_space(~s,V1)} end ",
		  [S, A, M]);
%% io_lib:format("epeg_pc:litthenspace(~p,~w)",[S,A]);
compile({lit,S}, _M, _N) ->
    %% think
    A = list_to_atom(S),
    io_lib:format("fun(~p ++ V1) -> {~p, V1} end ",[S,A]);
%% io_lib:format("epeg_pc:lit(~p,~w)",[S,A]);
compile({do,X,{action,Y}}, M, N) ->
    Head = make_head(X),
    ["epeg_pc:do(",compile(X,M,N), "," , Head, io_lib:format("~s",[Y]), "end )"];
compile({star,L}, M, N) ->
    ["epeg_pc:star(",compile(L,M,N),")"];
compile({plus,L}, M, N) ->
    ["epeg_pc:plus(",compile(L,M,N),")"];
compile({bang,L}, M, N) ->
    ["epeg_pc:bang(",compile(L,M,N),")"];
compile({question,L}, M, N) ->
    ["epeg_pc:question(",compile(L,M,N),")"];
compile({nt,X}, _, _) ->
    ["fun ",name(X),"/1"];
compile({nt,Mod,Fun}, _, _) ->
    ["fun ",Mod,":",name(Fun),"/1"];

compile({class,C}, _, _) ->
    io_lib:format("epeg_pc:cc(~p)",[C]);
compile(any, _, _) ->
    ["epeg_pc:any()"];
compile({seq,[X]}, M, N) ->
    compile(X, M, N);
compile({seq,L}, M, N) ->
    ["epeg_pc:seq([\n",indent(N+4),
     interlieve([",\n", indent(N+4)], [compile(I, M, N+4) || I <- L]),"])"];
compile({alt,[X]}, M, N) ->
    compile(X, M, N);
compile({alt,L}, M, N) ->
    ["epeg_pc:alt([\n",indent(N+4),
     interlieve([",\n",indent(N+4)], [compile(I, M, N+4) || I <- L]),"])"];
compile(C, _, _) ->
    io:format("cannot compile:~p~n",[C]),
    "".

indent(0) -> [];
indent(N) -> [$\s|indent(N-1)].


make_head({seq, L}) ->
    N = length(L),
    V = lists:reverse(mkVars(N)),
    V1 = interlieve(",", V),
    ["fun([",V1,"]) ->"];
make_head(_) ->
    "fun(V) -> ".


mkVars(0) -> [];
mkVars(N) -> ["V" ++ integer_to_list(N) | mkVars(N-1)].

name([$-|T]) -> "'parse_-" ++ T ++ "'";
name([$'|T]) -> "'parse_" ++ T;
name(X) -> "parse_" ++ X.

name1([$-|T]) -> "'-" ++ T ++ "'";
name1([$'|T]) -> "'" ++ T;
name1(X) -> X.

interlieve(_, [H]) -> [H];
interlieve(_, []) -> [];
interlieve(X, [H|T]) -> [H,X,interlieve(X, T)].

header(Mod) ->
    ["-module(",Mod,").
-compile(export_all).
-import(lists, [map/2, reverse/1]).

"].

pp(I, X) ->
    S = lists:flatten(pp1(I, X)),
    F = lists:flatten(io_lib:format("Abstract syntax:~n  ~p~n", [X])),
    [comment(S), comment(F)].

comment(F) ->
    ["%% ",comment1(F)].

comment1([$\n|T]) -> ["\n%% "|comment1(T)];
comment1([H|T]) ->  [H|comment1(T)];
comment1([]) -> ["\n\n"].

pp1(I, X) ->
    ["-------------- begin ----------------\n",
     "DEFN ", I, " = ", pp2(X), " ;"].

pp2({alt,L}) ->  interlieve(" | ", [pp2(I) || I <- L]);
pp2({seq,L}) ->  interlieve(" ", [pp2(I) || I <- L]);
pp2({star, X}) -> [body(X),"*"];
pp2({question,X}) -> [body(X),"?"];
pp2({plus,X}) -> [body(X),"+"];
pp2({bang,X}) -> ["!",body(X)];
pp2(X) -> body(X).

body({nt, X}) -> X;
body({nt, Mod,X}) -> [Mod,":",X];
body({litthenspace,X}) -> [$',X,$'];
body({lit,X}) -> [$",X,$"];
body({Tag,_} = S) when Tag == seq; Tag == alt; Tag == star;
		       Tag == question; Tag == plus
		       -> ["(", pp2(S), ")"];
body({class,C}) -> ["[",pp_class(C),"]"];
body(any) -> ".";
body(X) ->
 io_lib:format("Uggh:~p",[X]).

pp_class([{A,B}|T]) -> [A,$-,B|pp_class(T)];
pp_class([H|T]) -> [H|pp_class(T)];
pp_class([]) -> [].

%%----------------------------------------------------------------------
%% finaliser module

finaliser_module({gram, L}=G) ->
    case [Mod || {defn,"final", {nt,Mod}} <- L] of
	[M] ->
	    L1 = remove_final_defn(L),
	    {M, {gram, L1}};
	[] ->
	    {"?MODULE", G}
    end.

remove_final_defn([{defn,"final",_}|T]) -> T;
remove_final_defn([H|T]) -> [H|remove_final_defn(T)];
remove_final_defn([]) -> [].


