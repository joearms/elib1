%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(elib1_indexer_results).
-compile(export_all).
-import(elib1_misc, [time_fun/2]).

test() ->
    q(medium, "hypot lists").

q(Name, Str) ->
    time_fun("query",
	     fun() ->
		     Dets = atom_to_list(Name) ++ ".index",
		     {ok, result} = dets:open_file(result, {file,Dets}),
		     Words = string:tokens(Str, " "),
		     L1 = [q1(I) || I <- Words],
		     L2 = intersection(L1),
		     FileNames = [filename(I) || I <- L2],
		     dets:close(result),
		     io:format("~p~n",[FileNames])
	     end).

intersection(L) ->
    sets:to_list(sets:intersection([sets:from_list(I) || I <- L])).

filename(I) ->
    case dets:lookup(result, I) of
	[] -> error;
	[{I,X}] -> X
    end.

q1(Str) ->
    %% io:format("lookup:~p~n",[Str]),
    Bin1 = list_to_binary(elib1_porter:stem(Str)),
    L = case dets:lookup(result, Bin1) of
	    [] -> [];
	    [{_,Bin2}] ->
		elib1_gamma:gamma_to_alist(Bin2)
	end,
    io:format("~s : ~p hits~n",[Str, length(L)]),
    L.
