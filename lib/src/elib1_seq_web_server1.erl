%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(elib1_seq_web_server1).

%% %% callback module for a web server

%% Web servers are a wee bit complicated - mainly due to a number of
%% implicit assumptions that we make about the concurrency models used in
%% the server. Virtually all serious web sites make use of some kind of
%% back-end storage (a database) - so we have to consider the problem of
%% consistently updating the database. if the database is
%% non-transactional this is a particular problem.

%% Access to the web-server can be sequential or parallel. The
%% sequential mode is the simplest. One process at a time is allowed to
%% access the database. If multiple simultaneous processes are allowed to
%% access the data bases, then we will have to solve a number of rather
%% tricky problem to do with transactional consistency.

%% This server takes the simplest possible approach
%% There is One sequential server
%% There is One sequential K-V store
%% There are N http_drivers (parsing and displaying HTTP is done in parallel)
%% There is one TM manager per query connected to the K-V store
%% which does not have transaction semantics

%% Semantics of a transaction:
%%   1) Create a new transaction context for each request
%%   2) Reads go to the real store and are cached in the TM
%%   3) Writes go to the TM and the data is marked as dirty
%%   4) If the request does not crash (as caught by "catch ,...")
%%      all dirty data is written back to the real store

%%
%%                 +-------------+
%% -- socket 1 --->| http_driver |-----------+
%%                 +-------------+           |
%%                                           |
%%                 +-------------+           |
%% -- socket 2 --->| http_driver |-----------+
%%                 +-------------+           |
%%                                           |
%%                 +-------------+           |
%% -- socket 3 --->| http_driver |-----------+
%%                 +-------------+           |
%%                                           |
%%                 +-------------+           |
%% -- socket 4 --->| http_driver |-----------+
%%                 +-------------+           |
%%                                           |
%%            +------------------------------+
%%            |
%%            |     +---------------+           +-------------+
%%            +-----|   server      |-- 1:N ----|  TM manager |----+
%%                  +---------------+           +-------------+    |
%%                                                                 |
%%                  +-----------------------+                      |
%%                  |   KV persistent store |----------------------+
%%                  +-----------------------+
%%
%%
%% This is a stateless server (all state is in the DB)
%% ie there is no local state

-export([start/3, default_response/2, pre/1]).

-import(elib1_http_driver, [classify_file/1, header/1]).

-import(lists, [map/2]).

start(Mod, DB, Args) ->
    spawn_link(fun() -> start_server(Mod, DB, Args) end).

start_server(Mod, DBMod, Args) ->
    %% start the DB
    DBPid  = DBMod:start_link(Args),
    Port   = Mod:server_port(),
    Root   = Mod:root(),
    process_flag(trap_exit, true),
    Pid = spawn_link(fun() -> server(Root, DBPid, Mod) end),
    elib1_http_driver:start(Port, Pid),
    loop().

loop() ->
    receive
	Any ->
	    io:format("server:~p~n",[Any]),
	    loop()
    end.

mk_handler_fun(Pid, Root) ->
    fun(root) -> Root;
       (X) -> elib1_misc:rpc(Pid, X)
    end.

server(Root, DBPid, Mod) ->
    receive
	{Client, request, Request} ->
	    %% Make a transaction context
	    PidT = spawn_link(fun() -> transaction_context(DBPid) end),
	    F = mk_handler_fun(PidT, Root),
	    trace_request(Client, Request),
	    Ret = (catch Mod:handle(Request, F)),
	    %% io:format("Ret=~p~n",[Ret]),
	    case Ret of
		{'EXIT', Why} ->
		    PidT ! abort,
		    Client ! {self(), show({error,Why})};
		{ok, X, Body} when is_atom(X) ->
		    PidT ! commit,
		    Headers = header(X),
		    Client ! {self(), {Headers,Body}};
		{ok, Headers, Body} when is_list(Headers) ->
		    PidT ! commit,
		    Client ! {self(), {Headers,Body}};
		{error, What} ->
		    PidT ! abort,
		    %% format the error
		    Response = {header(text),
				[body("red"),pre(What)]},
		    Client ! {self(), Response};
		close ->
		    PidT ! abort,
		    Client ! {self(), close},
		    exit(closed);
		Other ->
		    PidT ! abort,
		    Client ! {self(),
			      show({internalError,handlerBadPattern,Other})}
	    end,
	    server(Root, DBPid, Mod);
	Any ->
	    io:format("server dropping:~p~n",[Any]),
	    server(Root, DBPid, Mod)
    end.

trace_request(Client, {Op,_Vsn,Uri,Args,_Env}) ->
    io:format("server  request:~p~n",[{Client, Op,Uri,Args}]).

transaction_context(Pid) ->
    process_flag(trap_exit, true),
    transaction_loop(Pid, []).

transaction_loop(Pid, L) ->
    receive
	{From, {read,Key}=Op} ->
	    case lists:keysearch(Key, 1, L) of
		{value, {Key, _Status, Val}} ->
		    From ! {self(), Val},
		    transaction_loop(Pid, L);
		false ->
		    %% ask the real DB
		    Val = elib1_misc:rpc(Pid, Op),
		    From ! {self(),Val},
		    L1 = [{Key,clean,Val}|L],
		    transaction_loop(Pid, L1)
	    end;
	{From, {write, Key, Val}} ->
	    L1 = lists:keystore(Key, 1, L, {Key,dirty,Val}),
	    From ! {self(), ack},
	    transaction_loop(Pid, L1);
	{From, info} ->
	    Info = elib1_misc:rpc(Pid, info),
	    Info1 = [transaction_manager|Info],
	    From ! {self(), Info1},
	    transaction_loop(Pid, L);
	{From, keys} ->
	    Vals = elib1_misc:rpc(Pid, keys),
	    From ! {self(), Vals},
	    transaction_loop(Pid, L);
	abort ->
	    void;
	commit ->
	    [elib1_misc:rpc(Pid, {write,K,V}) || {K,dirty,V} <- L],
	    void;
	Other ->
	    io:format("Unexpected:~p~n", [Other]),
	    void
    end.

default_response(F, Args) ->
    io:format("Read:~p~n",[F]),
    case file:read_file(F) of
        {ok, Bin} ->
            Type = classify_file(F),
	    {ok, Type, [Bin]};
        _ ->
            {error, {no_such_file,F,args,Args,cwd,file:get_cwd()}}
    end.

body(X) -> ["<body bgcolor=\"", X, "\">"].

show(X) ->
    {header(text),[body("white"),pre(X)]}.

pre(X) ->
    ["<pre>",
     quote_lt(lists:flatten(io_lib:format("~p~n",[X]))),
     "</pre>"].


quote_lt([$<|T]) -> "&lt;" ++ quote_lt(T);
quote_lt([H|T])  -> [H|quote_lt(T)];
quote_lt([])     -> [].


