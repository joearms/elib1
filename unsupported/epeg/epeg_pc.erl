%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(epeg_pc).

%% peg parser combinators

-export([alt/1, any/0, bang/1, block/1, cc/1, char/1, 
	 do/2, done/3, lit/1, lit/2, litthenspace/1, litthenspace/2,
	 plus/1, question/1, star/1, seq/1]).

-export([drop_space/1, drop_space/2]).

-import(lists, [reverse/1]).

%% higher order functions for building parsers
%%  all of these return funs of type fun(Str) -> {Val, Str'} raises Error

alt(L)       -> fun(Str) -> match_alt(L, Str) end.
any()        -> fun(Str) -> match_any(Str) end.
bang(X)      -> fun(Str) -> match_bang(X, Str) end.
block(F)     -> fun(Str) -> match_block(F, Str) end.
cc(X)        -> fun(Str) -> match_cc(X, Str) end.
do(P, Fun)   -> fun(Str) -> match_then_do(P, Fun, Str) end.
char(Char)   -> fun(Str) -> match_char(Char, Str) end.
lit(X)       -> fun(Str) -> match_lit(X, Str, X) end.
lit(X,Y)     -> fun(Str) -> match_lit(X, Str, Y) end.
    
litthenspace(X)    -> fun(Str) -> match_lit_then_space(X, Str) end.
litthenspace(X, Y) -> fun(Str) -> match_lit_then_space(X, Y, Str) end.
plus(X)      -> fun(Str) -> match_plus(X, Str) end.
question(X)  -> fun(Str) -> match_question(X, Str) end.
star(X)      -> fun(Str) -> match_star(X, Str, []) end.
seq(L)       -> fun(Str) -> match_seq(L, Str, []) end.

done(Mod, Tag, Val) ->
    case(catch apply(Mod, final, [Tag, Val])) of
	{'EXIT', Why} ->
	    io:format("fatal error in parser: ~w:~w~n"
		      "Val = ~p Error = ~p~n",[Mod, final, Val, Why]),
	    exit(fatal);
	Val1 ->
	    Val1
    end.

%% internal functions

match_any([H|T]) -> {H, T};
match_any([])    -> exit(fail).

match_block(F, Str) ->
    {_, Str1} = F(Str),
    Inner = find_head(Str, Str1, []),
    {Inner, Str1}.

match_then_do(Parser, Fun, Str) ->
    {Val, Str1} = Parser(Str),
    %% This is useful when debugging
    case (catch Fun(Val)) of
	{'EXIT', Why} ->
	    io:format("do failed on value:~p Why=~p~n",
		      [Val, Why]),
	    {oops, Str1};
	Result ->
	    {Result, Str1}
    end.

find_head(Rest, Rest, L)  -> reverse(L);
find_head([H|T], Rest, L) -> find_head(T, Rest, [H|L]).
	    
%% seq is actuall mapfold

match_seq([H|T], Str, L) ->
    {Val, Str1} = H(Str), 
    match_seq(T, Str1, [Val|L]);
match_seq([], Str, L) ->
    {reverse(L), Str}.

match_lit_then_space(X, Str) ->
    {Val, Str1} = match_lit(X, Str, X),
    Str2 = drop_space(Str1),
    {Val, Str2}.

match_lit_then_space(X, Y, Str) ->
    {Val, Str1} = match_lit(X, Str, Y),
    Str2 = drop_space(Str1),
    {Val, Str2}.

drop_space(Mod, Str) ->
    {_, Str1} = apply(Mod, 'parse_-', [Str]),
    Str1.

drop_space("\s" ++ T) -> drop_space(T);
drop_space("\n" ++ T) -> drop_space(T);
drop_space("\r" ++ T) -> drop_space(T);
drop_space("\t" ++ T) -> drop_space(T);
drop_space(T) -> T.

match_lit([], S, X)          -> {X, S};
match_lit([H|T1], [H|T2], X) -> match_lit(T1, T2, X).

match_char(H, [H|T]) -> {H, T}.

match_alt([H|T], S) ->
    case (catch H(S)) of
	{'EXIT', _} -> match_alt(T, S);
	Other       -> Other
    end.

%% 0 or more
match_star(F, S, L) ->
    case (catch F(S)) of
	{'EXIT', _} ->
	    {reverse(L), S};
	{Val, S1} ->
	    match_star(F, S1, [Val|L])
    end.

%% 1 or more
match_plus(F, S) ->
    case (catch F(S)) of
	{'EXIT', _} ->
	    exit(fail);
	{Val, S1} ->
	    match_star(F, S1, [Val])
    end.

%% 0 or 1
match_question(F, S) ->
    case (catch F(S)) of
	{'EXIT', _} -> {[], S};
	{Val, S1}   -> {[Val], S1}
    end.

match_bang(F, S) ->
    case (catch F(S)) of
	{'EXIT', _} -> {bang, S};
	_           -> exit(fail)
    end.


match_cc([], [])   -> {[], []};
match_cc(L, [H|T]) ->
    case matches(H, L) of
	true  -> {H, T};
	false -> exit(fail)
    end.

matches(H, [{Min,Max}|_]) when H >= Min, H =< Max -> true;
matches(H, [H|_]) -> true;
matches(H, [_|T]) -> matches(H, T);
matches(_, [])    -> false.
