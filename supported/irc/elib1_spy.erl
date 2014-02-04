%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(elib1_spy).

-export([start/0, stop/1, start/3]).
-compile(export_all).

start() ->
    start(2000,"irc.freenode.net", 6667).


start(LocalPort, RemoteHost, RemotePort) ->
    start_parallel_server(LocalPort,
			  fun(Socket) ->
				  server_start(Socket,
					       RemoteHost,
					       RemotePort)
			  end).

stop(Pid) ->
    stop_parallel_server(Pid).


%% @doc start(LocalPort, RemoteHost, RemotePort)
%% listens on LocalPort. When a connection to localhost:LocalPort is
%% established we connect to RemoteHost:RemotePort and log all the
%% messages. The log file is called LOG.RemoteHost-<N>

stop_parallel_server(Pid) ->
    exit(Pid, stop).

%% @doc start_parallel_server

%% -spec start_parallel_server(Port::integer(), fun(->_)) -> pid().


start_parallel_server(Port, Fun) ->
    S = self(),
    spawn_link(fun() ->
		       start_parallel_server0(S, Port, Fun)
	       end).

start_parallel_server0(Parent, Port, Fun) ->
    process_flag(trap_exit, true),
    Val = gen_tcp:listen(Port,
			 [binary, {packet,0},
			  {active, true},
			  {reuseaddr, true}]),
    io:format("listening to Port=~p Val=~p~n",[Port,Val]),
    {ok, Listen} = Val,
    S = self(),
    spawn_link(fun() -> par_connect(S, Listen, Fun) end),
    server_loop(S, Parent, Listen, Fun).

server_loop(Self, Parent, Listen, Fun) ->
    receive
	{'EXIT', Parent, Why} ->
	    io:format("parent exitted - that's it I die:~p~n",[Why]),
	    exit(iDie);
	startAnother ->
	    %% we got a connecting do it again
	    spawn_link(fun() -> par_connect(Self, Listen, Fun) end),
	    server_loop(Self, Parent, Listen, Fun);
	Other ->
	    io:format("unexpected:~p in ~p~n",
		      [Other, {Self,Parent,Listen,Fun}]),
	    server_loop(Self, Parent, Listen, Fun)
    end.

par_connect(Parent, Listen, Fun) ->
    io:format("waiting for a local connection~n"),
    {ok, Socket} = gen_tcp:accept(Listen),
    %% tell the parent so she can start another
    Parent ! startAnother,
    Fun(Socket).

%% Note it doesn't matter if we crash - the socket will be closed :-)

server_start(Local, Host, Port) ->
    io:format("trying to connect to: ~p ~p~n", [Host, Port]),
    %% case gen_tcp:connect("irc.freenode.net", 6667, [{active,true}], 20000) of
    case gen_tcp:connect(Host, Port, [{active,true}], 20000) of
	{ok, Remote} ->
	    io:format("we got it~n"),
	    Root = Host ++ "_" ++ integer_to_list(Port),
	    File = find_a_free_file(Root, ".LOG"),
	    io:format("opening log file:~p~n",[File]),
	    {ok, S} = file:open(File, [write]),
	    io:format(S, "~p.~n", [{opened, Host, Port}]),
	    io:format("Opening connection to ~p ~p~n",[Host, Port]),
	    log(Local, Remote, S);
	X ->
	    io:format("no remote connection:~p~n",[X]),
	    exit(eNoConnection)
    end.

log(Local, Remote, S) ->
    receive
	{tcp, Local, Data} ->
	    io:format(">> ~p~n", [Data]),
	    %% Data is a binary
	    io:format(S, "~p.~n",[{sent, binary_to_list(Data), Data}]),
	    gen_tcp:send(Remote, Data),
	    log(Local, Remote, S);
 	{tcp, Remote, Data} ->
	    io:format("<< ~p~n", [Data]),
	    %% Data here is a str
	    io:format(S, "~p.~n",[{received, Data, list_to_binary(Data)}]),
	    gen_tcp:send(Local, Data),
	    log(Local, Remote, S);
	{tcp_closed, Local} ->
	    io:format(S, "~p.~n",[{closed,local}]),
	    file:close(S),
	    io:format("Local terminated ~n");
	{tcp_closed, Remote} ->
	    io:format(S, "~p.~n",[{closed,remote}]),
	    file:close(S),
	    io:format("Remote terminated ~n")
    end.

-spec find_a_free_file(Base::string(), Extension::string()) ->
    FileName::string().

%% @doc Find a new filename. With a name like
%% File_<N>.Ext

find_a_free_file(File, Ext) ->
    find_a_free_file(File, Ext, 1).

find_a_free_file(Base, Ext, N) ->
    Full = Base ++ "_" ++ integer_to_list(N) ++ Ext,
    io:format("try ~p~n",[Full]),
    case filelib:is_file(Full) of
	true -> find_a_free_file(Base, Ext, N+1);
	false  -> Full
    end.
