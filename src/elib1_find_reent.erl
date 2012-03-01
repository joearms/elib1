-module(elib1_find_reent).
-compile(export_all).
-export([test1/0, foldfiles/3]).

%% foldfiles(Dir, Fun/3, Acc).
%%   calls Fun(F, Cost, Acc) -> Acc'
%%   for each file under Dir
%%   Cost is a real number from 0.0  1.0

test1() ->
    foldfiles("/Users/joe/code/elib2-1", fun listit/3, {0,0,[]}).

%% bug fails - I think it follows a symlink

bug() ->
    foldfiles("/Users/joe/code", fun listit/3, {0,0,[]}).

listit({file,F}, Cost,{Nf,ND,L}) ->
    io:format("File:~p ~p~n",[Cost,F]),
    {Nf+1,ND,L};
listit({dir,F}, Cost,{Nf,ND,L}) ->
    io:format("Dir:~p ~p~n",[Cost,F]),
    {Nf,ND+1,L};
listit({funny,F}, Cost,{Nf,ND,L}) ->
    io:format("File:~p ~p~n",[Cost,F]),
    {Nf,ND,[F|L]}.

foldfiles(Dir, F, Acc) ->
    I = make_file_iterator(Dir),
    fold(I, F, Acc).

fold(I, Fun, Acc) ->
    case step(I) of
	{File, Cost, I1} ->
	    Acc1 = Fun(File, Cost, Acc), 
	    fold(I1, Fun, Acc1);
	finished ->
	    Acc
    end.


-define(AVERAGE_FILES_PER_DIRECTORY, 4).
%% guess

make_filename(F,D,T) -> 
    %% io:format("Make_filename:~p~n",[{D,F,T}]),
    Path1 = [Dir || {Dir,_,_} <- T],
    Path2 = lists:reverse([F,D|Path1]),
    File = filename:join(Path2),
    %% io:format("examining:~p ~p~n",[Path2, File]),
    File.

step([{D,C,[{file,F}|T1]}|T2]) ->
    File = make_filename(F,D,T2),
    Cost = estimate([{D,C,T1}|T2]),
    {{file, File}, Cost, [{D,C,T1}|T2]};
step([{D,C,[{dir,F}|T1]}|T2]) ->
    File = make_filename(F,D,T2),
    {C1,L1} = list_dir_and_cost(File),
    S1 = [{F,C1,L1},{D,C,T1}|T2],
    Cost = estimate(S1),
    {{dir,File}, Cost, S1};
step([{D,C,[{{funny,Type},F}|T1]}|T2]) ->
    File = make_filename(F,D,T2),
    Cost = estimate([{D,C,T1}|T2]),
    {{funny,{Type,File}}, Cost, [{D,C,T1}|T2]};
step([{_,_,[]}|T]) ->
    step(T);
step([]) ->
    finished.

estimate(L) ->
    L1 = lists:reverse([{cost(Left),Max} || {_,Max,Left} <- L]),
    %% io:format("L1=~p~n",[L1]),
    estimate(L1, 1).

estimate([{_,0}|T], F) ->
    estimate(T, F);
estimate([{C,Max}|T], F) ->
    Complete = (Max - C) * F/Max,
    Complete + estimate(T, F/Max);
estimate([], _) ->
    0.

make_file_iterator(Dir) ->
    %% make the first iteration
    case filelib:is_dir(Dir) of
	true ->
	    {C,L} = list_dir_and_cost(Dir),
	    [{Dir, C, L}];
	false ->
	    exit({eNODir, Dir})
    end.

list_dir_and_cost(Dir) ->
    case file:list_dir(Dir) of
	{ok, Files} ->
	    %% io:format("Files=~p~n",[Files]),
	    L = [{classify(Dir, I),I} || I <- Files],
	    Cost = cost(L),
	    {Cost, L}; 
	{error, _} ->
	    io:format("** cannot list:~p~n",[Dir]),
	    {0, []}
    end.

cost(L) ->
    cost(L, 0).

cost([{file,_}|T], N) -> cost(T, N+1);
cost([{dir,_}|T], N) -> cost(T, N+?AVERAGE_FILES_PER_DIRECTORY); 
cost([_|T], N) -> cost(T, N); 
cost([], N) -> N.

classify(Dir, I) ->
    Full = filename:join(Dir, I),
    case elib1_misc:file_type(Full) of
	directory -> 
	    dir;
	regular ->
	    file;
	Other ->
	    {funny, Other}
    end.

	    
					     
    
