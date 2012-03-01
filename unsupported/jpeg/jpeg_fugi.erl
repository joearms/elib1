%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(jpeg).

-export([time/1]).

-import(lists, [filter/2, reverse/1, foreach/2]).

time(File) ->
    L = parse(File),
    %% io:format("L=~p~n",[L]),
    time1(L).

time1({ok, BB}) -> 
    case (catch get_time(binary_to_list(BB))) of
	error ->
	    error;
	X ->
	    {ok, X}
    end;
time1(error)    -> error.

get_time("Digital Camera FinePix4700 ZOOM Ver1.00\000" ++ T) ->
    extract_time(T);
get_time([_|T]) -> 
    get_time(T);
get_time([])    -> 
    error.

extract_time([0|_]) ->
    [];
extract_time([H|T]) ->
    [H|extract_time(T)];
extract_time([]) ->
    throw(error).


parse(File) ->
    {ok, Bin} = file:read_file(File),
    p(Bin).

p(Bin) ->
    case binary_head(Bin, 2) of
	{[16#ff,16#d8], B1} ->
	    p_seq(B1);
	fail ->
	    exit(not_jpg)
    end.

binary_head(Bin, Len) ->
    case split_binary(Bin, Len) of
	{'EXIT', _} -> fail;
	{B1, B2}    -> {binary_to_list(B1), B2}
    end.

p_seq(Bin) ->
    case binary_head(Bin, 2) of
	{[16#ff,16#da], B} -> 	error;
	_ ->
	    case binary_head(Bin, 4) of
		{[16#ff,Type,Msb,Lsb],T} ->
		    Len = (Msb bsl 8) bor Lsb,
		    {B1, B2} = split_binary(T, Len-2),
		    io:format("~p ~n",[Type]),
		    case Type of 
			225 ->
			    {ok, B1};
			_ ->
			    p_seq(B2)
		    end;
		fail ->
		    exit(fail)
	    end
    end.

   
