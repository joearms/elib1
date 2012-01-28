%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(elib1_file_finder).

-export([test/1, foldl/4]).

-compile(export_all).

-import(filename, [join/1]).
-import(lists, [member/2]).


-include_lib("kernel/include/file.hrl").


test(big) ->
    Top = join([os:getenv("HOME")]),
    L = foldl(Top, fun is_erl/4, [], fun follow/3),
    elib1_misc:dump("allfiles", L);
test(small) ->
    Top = join([os:getenv("HOME"), "code"]),
    L = foldl(Top, fun is_erl/4, [], fun follow/3),
    elib1_misc:dump("files", L).

is_erl(Top, Dir, File, L) ->
    Full = filename:join([Top, Dir, File]),
    case filename:extension(Full) of
	".erl" ->
	    %% io:format("(~w) Top=~p Dir=~p File=~p~n",[N+1, Top, Dir, File]),
	    [{Dir,File}|L];
	_ ->
	    L
    end.

print_file(Top, Dir, File, N) ->
    io:format("(~w) Top=~p Dir=~p File=~p~n",[N+1, Top, Dir, File]),
    N+1.

follow(_Top, _Dir, H) ->
    case H of
	"." ++ _ ->
	    %% io:format("rejecting:~p~n",[join([Top,Dir,H])]),
	    false;
	_ ->
	    true
    end.

%% foldl(Top, F, A, Follow) -> A'
%%  Finds all files which are subdirectories
%%  of Top, following symbolic
%%  For each file that is found
%%  F(FullName, A) -> A' is called
%%  To decide if to recurse into a sub-directory
%%  Follow(Top, Dir, File) -> Bool is called
%%   if Follow returns true then the sub-directory is
%%   followed otherwise not

foldl(Top, F, A, Follow) ->
    case file:list_dir(Top) of
	{ok, Files} ->
	    %% io:format("Files=~p~n",[Files]),
	    fold_files(Files, Top, [], F, A, Follow, []);
	{error, _}  -> A
    end.

fold_files([], _Top, _Dir, _F, A, _Follow, _Path) ->
    A;
fold_files([H|T], Top, Dir, F, A, Follow, Path) ->
    FullName = filename:join([Top,Dir,H]),
    %% io:format("here1:A=~p ~p ~p~n",[A,FullName,{Top,Dir,H}]),
    case file_type(FullName) of
	error ->
	    fold_files(T, Top, Dir, F, A, Follow, Path);
	link ->
	    case contains_dotdot(FullName) of
		true ->
		    io:format("** symlink ~p not followed~n",[FullName]),
		    fold_files(T, Top, Dir, F, A, Follow, Path);
		false ->
		    case member(FullName, Path) of
			true ->
			    io:format("** circular symlink ~p not followed~n",
				      [FullName]),
			    fold_files(T, Top, Dir, F, A, Follow, Path);
			false ->
			    %% io:format("following symlink:~p~n",[FullName]),
			    A1 = fold_files(T, Top, Dir, F, A, Follow, Path),
			    Dir1 = filename:join([Dir,H]),
			    fold_files([], Top, Dir1, F, A1, Follow,[Dir1|Path])
		    end
	    end;
	directory ->
	    A1 = case follow(Top, Dir, H) of
		     true ->
			 Top1 = filename:join([Top, Dir, H]),
			 case file:list_dir(Top1) of
			     {ok, Files} ->
				 %% io:format("Files=~p~n",[Files]),
				 %% io:format("Dir:~p H=~p join=~p~n",
				 %% [Dir,H,join1(Dir,H)]),
				 fold_files(Files, Top, join1(Dir,H), F,
					    A, Follow, Path);
			     {error, _}  ->
				 A
			 end;
		     false ->
			 A
		 end,
	    fold_files(T, Top, Dir, F, A1, Follow, Path);
	regular ->
	    A1 = F(Top, Dir, H, A),
	    fold_files(T, Top, Dir, F, A1, Follow, Path)
    end.

join1([], H) -> H;
join1(D, H) -> join([D,H]).


contains_dotdot(".." ++ _) -> true;
contains_dotdot([]) -> false;
contains_dotdot(X) -> contains_dotdot(tl(X)).

file_type(File) ->
    case file:read_link(File) of
	{ok, _} ->
	    link;
	_ ->
	    case file:read_file_info(File) of
		{ok, Facts} ->
		    case Facts#file_info.type of
			regular   -> regular;
			directory -> directory;
			_         -> error
		    end;
		_ ->
		    error
	    end
    end.



