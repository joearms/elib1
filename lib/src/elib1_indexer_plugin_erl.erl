%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(elib1_indexer_plugin_erl).
%%
-export([file/1, strings/1, frequencies/1, tokenise_string/1]).
-import(lists, [foldl/3, member/2, reverse/1]).

-define(IN(X,Min,Max), X >= Min, X =< Max).
-define(WHITESPACE(X),X=:=$\s;X=:=$\t;X=:=$\r;X=:=$\n).
-define(IS_DIGIT(X),$0=<X,X=<$9).
-define(IS_ALPHA(X),$a=<X,X=<$z;$A=<X,X=<$Z).
-define(IS_LOWER(X),$a=<X,X=<$z).
-define(IS_UPPER(X),$A=<X,X=<$Z).

%% only interested in atoms and comments

file(F) ->
    {ok, B} = file:read_file(F),
    frequencies(binary_to_list(B)).

strings(Str) ->
    [S || {S,_} <- frequencies(Str)].

frequencies(Str) ->
    %% io:format("string=~p~n",[Str]),
    {Comments, Atoms} = tokenise_string(Str),
    %% io:format("Comments=~p~n",[Comments]),
    %% io:format("Atoms=~p~n",[Atoms]),
    {ok, Re} = re:compile("[^a-z]+"),
    Dict  = dict:new(),
    Dict1 = add_comments(Comments, Re, Dict),
    Dict2 = add_atoms(Atoms, Dict1),
    Words = dict:to_list(Dict2),
    %% io:format("Words with frequencies (unstemmed)=~p~n",[Words]),
    Words.

add_atoms(Atoms, Dict) ->
    foldl(fun add_atom/2, Dict, Atoms).

add_atom({_Ln, Atom}, Dict) ->
    %% atoms can contain _@ and be uppercaes
    Parts = string:tokens(elib1_misc:to_lower(Atom), "_@"),
    foldl(fun(Word, S) ->
		  case member(Word, stop()) of
		      true  -> S;
		      false -> add_element(Word, Dict)
		  end
	  end, Dict, Parts).

add_comments(Comments, Re, Dict) ->
    foldl(fun(C,S) -> add_comment(C, Re, S) end, Dict, Comments).

add_comment({_Ln,Str}, Re, Dict) ->
    Str1 = elib1_misc:to_lower(Str),
    Words = re:split(Str1, Re,  [{return,list},trim]),
    foldl(fun add_word/2, Dict, Words).

add_word([], Dict)   -> Dict;
add_word(Word, Dict) -> add_element(Word, Dict).

add_element(Word, Dict) when length(Word) =< 2 ->
    Dict;
add_element(Word, Dict) ->
    %% finally this is where we add stuff
    case dict:find(Word, Dict) of
	error ->
	    dict:store(Word, 1, Dict);
	{ok, N} ->
	    dict:store(Word, N+1, Dict)
    end.

tokenise_string(Str) -> string2toks(Str, 1, [], []).

string2toks([$%|T], Ln, Cs, As) ->
    {Comment,T1} = collect_comment(T, []),
    string2toks(T1, Ln, [{Ln,Comment}|Cs], As);
string2toks([H|T], Ln, Cs, As) when ?IS_LOWER(H) ->
    {Atom, T1} = collect_atom(T, [H]),
    string2toks(T1, Ln, Cs, [{Ln,Atom}|As]);
string2toks([H|T], Ln, Cs, As) when ?IS_UPPER(H) ->
    %% variable -- treat like an atom and throw away the rest
    {_Atom, T1} = collect_atom(T, [H]),
    string2toks(T1, Ln, Cs, As);
string2toks([$"|T], Ln, Cs, As) ->
    {Ln1, T1} = skip_string(T, $", Ln),
    string2toks(T1, Ln1, Cs, As);
string2toks([$'|T], Ln, Cs, As) ->
    {Ln1, T1} = skip_string(T, $', Ln),
    string2toks(T1, Ln1, Cs, As);
string2toks([$\n|T], Ln, Cs, As) ->
    string2toks(T, Ln+1, Cs, As);
string2toks([_|T], Ln, Cs, As) ->
    string2toks(T, Ln, Cs, As);
string2toks([], _, Cs, As) ->
    {Cs, As}.

collect_atom([H|T], L) when ?IS_ALPHA(H) ; ?IS_DIGIT(H) ->
    collect_atom(T, [H|L]);
collect_atom([$_|T], L) ->
    collect_atom(T, [$_|L]);
collect_atom(T, L) ->
    {reverse(L), T}.

collect_comment("\r\n" ++ T, L) -> {reverse(L), T};
collect_comment("\n" ++ T, L)   -> {reverse(L), T};
collect_comment([], L)          -> {reverse(L), []};
collect_comment([H|T], L)       -> collect_comment(T, [H|L]).

skip_string([H|T], H, Ln)      -> {Ln, T};
skip_string([], _, Ln)         -> {Ln, []};
skip_string([$\n|T], Stop, Ln) -> skip_string(T, Stop, Ln+1);
skip_string([_|T], Stop, Ln)   -> skip_string(T, Stop, Ln).


stop() ->
    ["module","if","case","receive","end","true","false","try","of","case"].
