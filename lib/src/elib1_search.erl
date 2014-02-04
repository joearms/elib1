%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(elib1_search).
-compile(export_all).
-import(elib1_mysql, [start/4, cmd/2, stop/1]).

make_data_base() ->
    {ok, Pid} = start("localhost", 3306,
		      password:username(mysql), password:password(mysql)),
    cmd(Pid, "use test"),
    cmd(Pid, "drop table if exists mods"),
    cmd(Pid, "create table if not exists mods ("
	     "themod CHAR(32) not null primary key,"   %% don't call this mod
             "val mediumtext not null, fulltext(val))"),
    _V1 = cmd(Pid, "show tables"),
    V2 = cmd(Pid, "describe mods"),
    stop(Pid),
    V2.

insert_erl() ->
    _Files = find:files(".", "*.erl", true),
    a.



