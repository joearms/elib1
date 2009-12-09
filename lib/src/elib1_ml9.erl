%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

%% title markup language 9 - generalised markup language
%% end

-module(elib1_ml9).

%% Time-stamp: <2009-11-19 10:47:54 ejoearm>

-export([doc/0,
	 run/1,
	 batch/1,
	 break_into_paras/1,
	 fetch/2,  
	 search/2, 
	 parse_file/1, 
	 parse_binary/1, 
	 parse_string/1, 
	 parse_para/1]).

-import(lists, [member/2, map/2, reverse/1, reverse/2]).

-define(IN(X,Min,Max), X >= Min, X =< Max).
-define(DIGIT(X), X >= $0, X =< $9).
-define(LETTER(X), X >= $a, X =< $z ;  X >= $A, X =< $Z).
-define(UPPER(X),  X >= $A, X =< $Z).

doc() ->
    run("doc.ml9").

batch([A]) ->
    run(atom_to_list(A)),
    init:stop().

run(File) ->
    case parse_file(File) of
	{ok, L} ->
	    do_auto_exec(L, L, File);
	{error, N} ->
	    io:format("~w errors in parse~n",[N]),
	    true
    end.

%% by here all includes will have been removed
do_auto_exec([{autoexec,A}|T], L, File) ->
    autoexec(A, L, File),
    do_auto_exec(T, L, File);
do_auto_exec([{comment,_}|T], L, File) ->
    do_auto_exec(T, L, File);
do_auto_exec(_, _, _) ->
    true.

autoexec(Assoc, L, File) ->
    case search(mod, Assoc) of
	{ok, Mod} ->
	    %% io:format("Autoexec:~p~n",[Mod]),
	    Mod:exec(File, L);
	error ->
	    io:format("*** error autoexec no module~n")
    end.

fetch(Key, Assoc) ->
    ok(search(Key, Assoc)).

ok({ok, X}) -> X.
    
search(Key, [{Key,Val}|_]) -> {ok, Val};
search(Key, [_|T])         -> search(Key, T);
search(_, [])              -> error.

parse_file(File) ->
    %% io:format("Parse file:~s~n",[File]),
    {ok, B} = file:read_file(File),
    parse_binary(B).

parse_binary(B) ->
    case (catch parse_string(binary_to_list(B))) of
	{'EXIT', Why} ->
	    {error, Why};
	Ok ->
	    Ok
    end.

parse_string(Str) ->
    Chunks = break_into_chunks(Str, 1, []),
    %% io:format("Chunks=~n~p~n", [Chunks]),
    Parse  = map(fun parse_chunk/1, Chunks),
    %% io:format("Parse=~p~n",[Parse]),
    Parse1 = do_includes(Parse),
    Nerrors = length([error||error <- Parse1]),
    case Nerrors of
	0 ->
	    {ok, Parse1};
	N -> 
	    {error, N}
    end.

do_includes([{include,X}|T]) ->
    case search(data, X) of
	{ok, {N, File}} ->
	    File1 = trim_filename(File),
	    io:format("Including file=~p~n",[File1]),
	    case parse_file(File1) of
		{ok, Parse1} ->
		    Parse1 ++ do_includes(T);
		{error, N} ->
		    [error|do_includes(T)]
	    end;
	error ->
	    io:format("no file to include~n"),
	    [error|do_includes(T)]
    end;
do_includes([H|T]) ->
    [H|do_includes(T)];
do_includes([]) ->
    [].

trim_filename([$\s|T]) -> trim_filename(T);
trim_filename(T) ->  reverse(trim_junk(reverse(T))).

trim_junk([$\s|T]) -> trim_junk(T);
trim_junk([$\r|T]) -> trim_junk(T);
trim_junk([$\n|T]) -> trim_junk(T);
trim_junk(X) -> X.
    
parse_chunk({N, [$@|T]}) ->
    case collect_header(T) of
	{ok, Tag, T1} ->
	    %% io:format("Tag=~p T1=~p~n",[Tag,T1]),
	    {Toks, N1, T2} = tokenise(T1, N),
	    case Toks of
		[] ->
		    {Tag, [{data,{N1,T2}}]};
		_ ->
		    %% io:format("Toks=~p~n",[Toks]),
		    case elib1_ml9_parse_header:parse(Toks) of
			{ok, Val} ->
			    {Tag, [{data,{N1,T2}}|Val]};
			{error, {Ln,M,A}} ->
			    io:format("error in tag on line:~w ~s~n",
				      [Ln,M:format_error(A)]),
			    io:format("Toks=~p~n",[Toks]),
			    error
		    end
	    end;
	error ->
	    error
    end.

%% header follows @
%%   and is an Atom

collect_header(Str) ->
    case collect_atom(Str, []) of
	{[], _} ->
	    error;
	{Atom1, T} ->
	    {ok, Atom1, T}
    end.

tokenise(T, N) ->
    %% peep to see if we have a {
    case skip_blanks(T) of
	[${|_] ->
	    tokenise(T, 0, N, []);
	_ ->
	    {[], N,T}
    end.

tokenise([$}|T], 1, N, L) ->
    %% top level } -- finishes the tokens 
    {reverse([{'}',N}|L]), N, T};
tokenise([$}|T], Level, N, L) ->
    %% top level } -- finishes the tokens 
    tokenise1(T, Level-1,N, [{'}',N}|L]);
tokenise([${|T], Level, N, L) ->
    %% top level } -- finishes the tokens 
    tokenise1(T, Level+1,N, [{'{',N}|L]);
tokenise([H|T], Level, N, L) when ?IN(H, $0, $9) ->
    {Int, T1} = collect_integer(T, H - $0),
    tokenise1(T1, Level, N, [{int,N,Int}|L]);
tokenise([$-,H|T], Level, N, L) when ?IN(H, $0, $9) ->
    {Int, T1} = collect_integer(T, H - $0),
    tokenise1(T1, Level, N, [{int,N,-Int}|L]);
tokenise([H|T], Level, N, L) when ?IN(H, $a, $z) ->
    {Atom, T1} = collect_atom(T, [H]),
    tokenise1(T1, Level, N, [{atom,N,Atom}|L]);
tokenise([$[|T], Level, N, L) ->
    tokenise1(T, Level, N, [{'[',N}|L]);
tokenise([$]|T], Level, N, L) ->
    tokenise1(T, Level, N, [{']',N}|L]);
tokenise([$=|T], Level, N, L) ->
    tokenise1(T, Level, N, [{'=',N}|L]);
tokenise([$,|T], Level, N, L) ->
    tokenise1(T, Level, N, [{',',N}|L]);
tokenise([$"|T], Level, N, L) ->
    {Str,N1,T1} = collect_string(T, $", N, []),
    tokenise1(T1,Level,N1,[{string,N,Str}|L]);
tokenise([$'|T], Level, N, L) ->
    {Str,N1,T1} = collect_string(T, $', N, []),
    tokenise1(T1, Level, N1,[{string,N,Str}|L]);
tokenise([$\t|T], Level, N, L) ->
    tokenise1(T, Level, N, L);
tokenise([$\s|T], Level, N, L) ->
    tokenise1(T, Level, N, L);
tokenise([$\r|T], Level, N, L) ->
    tokenise1(T, Level, N, L);
tokenise([$\n|T], Level, N, L) ->
    tokenise1(T, Level, N+1, L);
tokenise([$%|T], Level, N, L) ->
    T1 = skip_to_eol(T),
    tokenise1(T1, Level, N, L);
tokenise([H|_], _Level, N, _L) ->
    io:format("unrecognised character \"~c\" in line ~w~n",
	      [H, N]),
    throw(error).

tokenise1(T, Level, N, Toks) ->
    %% io:format("---~ntokenise1: T=~p~nLevel=~p~nToks=~p~n",[T,Level,Toks]),
    tokenise(T, Level, N, Toks).

skip_blanks([$\s|T]) -> skip_blanks(T);
skip_blanks([$\t|T]) -> skip_blanks(T);
skip_blanks(X)       -> X.

skip_to_eol(X=[$\n|_]) -> X;
skip_to_eol([_|T])     -> skip_to_eol(T);
skip_to_eol(X)         -> X.
    
collect_integer([H|T], N) when ?IN(H, $0, $9) ->
    collect_integer(T, N*10+H-$0);
collect_integer(S, N) ->
    {N, S}.

break_into_chunks([], _, L) ->
    fix_first_chunk(reverse(L));
break_into_chunks(Str, N, L) ->
    {Chunk, N1, Str1} = collect_chunk(Str, N, []),
    break_into_chunks(Str1, N1, [{N, Chunk}|L]).

fix_first_chunk(X = [{_N,"@" ++ _}|_]) -> X;
fix_first_chunk([{N,_Str}|T])          -> [{N,"@comment "}|T];
fix_first_chunk(X)                     -> X.

collect_chunk("\r\n@" ++ T, N, L) -> {reverse(L), N+1, [$@|T]};
collect_chunk("\n@" ++ T, N, L)   -> {reverse(L), N+1, [$@|T]};
collect_chunk([], N, L)           -> {reverse(L), N, []};
collect_chunk([$\n|T], N, L)      -> collect_chunk(T, N+1, [$\n|L]);
collect_chunk([H|T], N, L)        -> collect_chunk(T, N, [H|L]).
    
collect_atom([H|T], L) when ?IN(H,$a,$z) -> collect_atom(T, [H|L]);
collect_atom([H|T], L) when ?IN(H,$A,$Z) -> collect_atom(T, [H|L]);
collect_atom([H|T], L) when ?IN(H,$0,$9) -> collect_atom(T, [H|L]);
collect_atom([$-|T], L) -> collect_atom(T, [$-|L]);
collect_atom([$_|T], L) -> collect_atom(T, [$_|L]);
collect_atom(T, L)      -> {list_to_atom(reverse(L)), T}.

%% within a string we can quote only the stop character

collect_string([S|T], S, N, L)     -> {reverse(L), N, T};
collect_string([$\\,S|T], S, N, L) -> collect_string(T, S, N, [S,$\\|L]);  
collect_string([$\n|T], S, N, L)   -> collect_string(T, S, N+1, [$\n|L]);  
collect_string([H|T], S, N, L)     -> collect_string(T, S, N, [H|L]);
collect_string([], S, N, L) ->
    io:format("** Warning missing stop character (~c) at end of string in line:~w~n",
	      [S, N]),
    {reverse(L), N, []}.

%% inline parser
parse_para(Str) ->
    L = break_into_paras(Str),
    map(fun(I) -> parse_para_content(I, []) end, L).


break_into_paras(S) ->
    cleanup(break_into_paras0(S)).

cleanup([[]|T]) -> cleanup(T);
cleanup([H|T])  -> [trim(H)|cleanup(T)];
cleanup([])     -> [].

trim([$\n|T]) -> T;
trim(X) -> X.


break_into_paras0([]) ->
    [];
break_into_paras0(Str) ->
    {Para, Str1} = collect_para(Str, []),
    [Para|break_into_paras0(Str1)].

collect_para([$\n|T], L) ->
    case all_blank_before_next_nl(T) of
	true ->
	    {reverse(L), T};
	false ->
	    collect_para(T, [$\n|L])
    end;
collect_para([H|T], L) ->
    collect_para(T, [H|L]);
collect_para([], L) ->
    {reverse(L), []}.

all_blank_before_next_nl([$\n|_]) -> true;
all_blank_before_next_nl([$\s|T]) -> all_blank_before_next_nl(T);
all_blank_before_next_nl([$\r|T]) -> all_blank_before_next_nl(T);
all_blank_before_next_nl([$\t|T]) -> all_blank_before_next_nl(T);
all_blank_before_next_nl([])      -> true;
all_blank_before_next_nl(_)       -> false.

%% parse the paragraph contents
%% This just isolates the inline elements

parse_para_content("''''" ++ T, L) ->
    {Str,T1} = collect_code(T, []),
    parse_para_content(T1, [{code,Str}|L]);
parse_para_content("'''" ++ T, L) ->
    {Str, T1} = collect_bold(T, []),
    parse_para_content(T1, [{bold,Str}|L]);
parse_para_content("''" ++ T, L) ->
    {Str, T1} = collect_italic(T, []),
    parse_para_content(T1, [{italic,Str}|L]);
parse_para_content("~" ++ T, L) ->
    {Word, T1} = collect_link(T, []),
    parse_para_content(T1, [{link,Word}|L]);
parse_para_content([H|T], L) ->
    {Str, T1} = collect_string(T, [H]),
    parse_para_content(T1, [{str,remove_nl(Str)}|L]);
parse_para_content([], L) ->
    reverse(L).

remove_nl([$\n|T]) -> T;
remove_nl(T)       -> T.

collect_string(X=[H|T], L) ->
    case start_of_inline(X) of
	true ->
	    {reverse(L), X};
	false ->
	    collect_string(T, [H|L])
    end;
collect_string([], L) ->
    {reverse(L), []}.


start_of_inline("~" ++ _) -> true;
start_of_inline("*" ++ _) -> true;
start_of_inline("''''" ++ _) -> true;
start_of_inline("'''" ++ _) -> true;
start_of_inline("''" ++ _) -> true;
start_of_inline(_) -> false.

collect_bold("'''" ++ T, L) -> {reverse(L), T};
collect_bold([H|T], L)      -> collect_bold(T, [H|L]);
collect_bold([], L)         -> {reverse(L), []}.

collect_code("''''" ++ T, L) -> {reverse(L), T};
collect_code([H|T], L)       -> collect_code(T, [H|L]);
collect_code([], L)          -> {reverse(L), []}.

collect_italic("''" ++ T, L) -> {reverse(L), T};
collect_italic([H|T], L)     -> collect_italic(T, [H|L]);
collect_italic([], L)        -> {reverse(L), []}.

collect_link([$\\,H|T], L) -> collect_link(T, [H|L]);
collect_link("~" ++ T, L)  -> {reverse(L), T};
collect_link([H|T], L)     -> collect_link(T, [H|L]);
collect_link([], L)        -> {reverse(L), []}.
    
