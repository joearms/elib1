%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(elib1_ensure_copyrighted).
-compile(export_all).

start() ->
    L = elib1_find:files(elib1_misc:root_dir(), "*.erl", true),
    [ensure_copyrighted(I, copyright()) || I <- L].

ensure_copyrighted(F, C) ->
    L = elib1_misc:file2lines(F),
    %% io:format("L=~p~n",[L]),
    fix(L, C, F).

fix([C1,C2|_], [C1, C2], File) ->
    io:format("~p ok~n", [File]);
fix(["%% Copyright " ++ _, C2|T], [C1,C2], File) ->
    %% change an old copyright
    create(File, [C1,C2,"\n"|T]);
fix(T, [C1,C2], File) ->
    create(File, [C1,C2,"\n"|T]).

create(File, L) ->
    io:format("Updating:~p~n",[File]),
    file:write_file(File, L).
    

copyright() ->
    ["%% Copyright (c) 2006-2009 Joe Armstrong\n",
     "%% See MIT-LICENSE for licensing information.\n"].
