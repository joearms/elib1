%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

%% % For licence read LICENCE in the top-level directory of this application
%% % (c) 2008 Joe Armstrong. All rights Reserved.
%% % Time-stamp: <2009-09-24 19:54:13 joe>
%% \title{elib1\_find: Find files}
%% \author{Joe Armstrong $<$joearms@gmail.com$>$}
%% \maketitle
%%  (c) 2008 Joe Armstrong. 
%%  All rights Reserved.\footnote{\tiny{\licenseMIT}}
%%
%% \section{Introduction}
%% A find utility.

-module(elib1_find).

-export([files/3, files/5, fold_all_files/3, file_type/1,
	 fold_files_with_excluded_dirs/4,
	 out_of_date/4]).
-import(lists, [map/2, reverse/1]).
-compile(export_all).
 
-include_lib("kernel/include/file.hrl").

%% Find all files in \verb+DirIn+ with extension \verb+ExtIn+
%% Check that there is a corresponnding file in \verb+DirOut+
%% with extension \verb+ExtOut+ -- that is later that the
%% input file.

out_of_date(DirIn, ExtIn, DirOut, OutAdd) ->
    L = files(DirIn, "*" ++ ExtIn, false),
    L1 = map(fun(In) ->
		     Name = filename:basename(In, ExtIn),
		     Out = filename:join([DirOut,Name]) ++ OutAdd,
		     case filelib:is_file(Out) of
			 false -> {yes, {In, Out}};
			 true ->
			     case {filelib:last_modified(In),
				   filelib:last_modified(Out)} of
				 {X, Y} when X > Y -> {yes, {In, Out}};
				 _                 -> no
			     end
		     end
	     end, L),
    [X || {yes, X} <- L1].

files(Dir, Re, Flag) -> 
    %% Re1 = regexp:sh_to_awk(Re),
    Re1 = xmerl_regexp:sh_to_awk(Re),
    reverse(files(Dir, Re1, Flag, fun(File, Acc) ->[File|Acc] end, [])).

files(Dir, Reg, Recursive, Fun, Acc) ->
    %% io:format("In:~p~n",[Dir]),
    case file:list_dir(Dir) of
	{ok, Files} -> find_files(Files, Dir, Reg, Recursive, Fun, Acc);
	{error, _}  -> Acc
    end.

find_files([File|T], Dir, Reg, Recursive, Fun, Acc0) ->
    FullName = filename:join([Dir,File]),
    case file_type(FullName) of
	link ->
	    %% io:format("** symlink ~p not followed~n",[FullName]),
	    find_files(T, Dir, Reg, Recursive, Fun, Acc0);
	regular ->
	    case re:run(FullName, Reg) of
		{match,_} ->
		    Acc = Fun(FullName, Acc0),
		    find_files(T, Dir, Reg, Recursive, Fun, Acc);
		_ ->
		    find_files(T, Dir, Reg, Recursive, Fun, Acc0)
	    end;
	directory -> 
	    case Recursive of
		true ->
		    Acc1 = files(FullName, Reg, Recursive, Fun, Acc0),
		    find_files(T, Dir, Reg, Recursive, Fun, Acc1);
		false ->
		    find_files(T, Dir, Reg, Recursive, Fun, Acc0)
	    end;
	error -> 
	    find_files(T, Dir, Reg, Recursive, Fun, Acc0)
    end;
find_files([], _, _, _, _, A) ->
    A.

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

fold_all_files(TopDir, Fun, Acc) ->
    Exclude = fun(_) -> false end,
    fold_files_with_excluded_dirs(TopDir, Exclude, Fun, Acc).

%% Starting from \verb+TopDir+ fold over all files in subdirectories
%% \verb+ExcludeDir+ is called with each new directory that is encountered
%% \verb+ Excluddir(Dir) -> true+ means the directory should be excluded.

-spec fold_files_with_excluded_dirs(TopDir::string(),
				    ExcludeDir::fun((Dir::string()) -> bool()),
				    fun((File::string(), Any) -> Any),
				    Any) -> Any.

fold_files_with_excluded_dirs(TopDir, ExcludeDir, Fun, Acc) ->
    case file:list_dir(TopDir) of
	{ok, Files} ->
	    fold_files(Files, TopDir, ExcludeDir, Fun, Acc);
	{error, _}  -> Acc
    end.

fold_files([File|T], Dir, ExcludeDir, Fun, Acc0) ->
    FullName = filename:join([Dir,File]),
    case file_type(FullName) of
	regular ->
	    Acc = Fun(FullName, Acc0),
	    fold_files(T, Dir, ExcludeDir, Fun, Acc);
	directory -> 
	    case ExcludeDir(FullName) of
		true ->
		    io:format("excluding:~p~n",[FullName]),
		    fold_files(T, Dir, ExcludeDir, Fun, Acc0);
		false ->
		    Acc1 = fold_files_with_excluded_dirs(FullName, ExcludeDir,
							 Fun, Acc0),
		    fold_files(T, Dir, ExcludeDir, Fun, Acc1)
	    end;
	error -> 
	    io:format("Funny:~p~n",[FullName]),
	    fold_files(T, Dir, ExcludeDir, Fun, Acc0)
    end;
fold_files([], _, _, _, A) ->
    A.


