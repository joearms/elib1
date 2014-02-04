%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(elib1_screen).

-compile(export_all).

%% Time-stamp: <2009-10-17 18:28:13 joe>

-import(lists, [duplicate/2, foreach/2]).

-define(IAC, 255). %% Interpret as command
-define(DO, 253).  %%
-define(WILL, 251).
-define(SB, 250). %% sub negociation Begin
-define(SE, 240). %% Sub negociation End
-define(ECHO, 1).
-define(NAWS, 31).
-define(SURPRESS_GO_AHEAD, 3).

%% \verb+http://en.wikipedia.org/wiki/ANSI_escape_code#Examples+

%% We start a registered server
%% When we start a window we first send a message
%% to the registered process with the Fun to be evaluated
%% the the telnet server asks the generic server for the Fun
%% then gets the Fun and evaluates it. The Port is srover in the generic server

test() ->
    make_clock(),
    make_window(fun handler1/1).

make_clock() ->
    make_window(fun clock/1).

clock(Pid) ->
    Pid ! clear_screen(),
    Pid ! goto(10,10),
    Time = lists:flatten(io_lib:format("Time:~p~n",[erlang:time()])),
    Pid ! Time,
    sleep(1000),
    clock(Pid).

sleep(T) ->
    receive
	after
	    T ->
		true
	end.



handler1(Pid) ->
    Pid ! "\e[32;40;1m",
    Pid ! clear_screen(),
    for(10,20, fun(I)->
		       Pid ! goto(I, I),
		       Pid ! "Wow"
	       end),
    handler_loop(Pid).

handler_loop(Pid) ->
    receive
	{char, C} ->
	    Pid ! [C],
	    handler_loop(Pid);
	Any ->
	    io:format("Any=~p~n",[Any]),
            handler_loop(Pid)
    end.

make_window(ServerFun) ->
    spawn(fun() ->
		  %% make sure there is a registered server running
		  elib1_misc:ensure_started(?MODULE, fun start_server/0),
		  Port = elib1_misc:rpc(?MODULE, {getPortTellFun, ServerFun}),
		  io:format("using port:~p~n", [Port]),
		  Cmd = case os:cmd("uname") of
			    "Darwin\n" ->
				elib1_misc:root_dir() ++
				    "/bin/term.sh -p 100,100 -s 600,400 -x telnet localhost ";
			    _ ->
				"gnome-terminal -x telnet localhost "
			end,
		  Cmd1 = Cmd ++ integer_to_list(Port),
		  io:format("Command=~p~n",[Cmd1]),
		  Val = os:cmd(Cmd1),
		  io:format("Val=~p~n",[Val]),
		  elib1_misc:forever()
	  end).

start_server() ->
    Port = start_telnet_server(),
    loop1(Port).

loop1(Port) ->
    receive
	{From, {getPortTellFun, F}} ->
	    From ! {?MODULE, Port},
	    loop2(Port, F);
	Other ->
	    io:format("unexpected:~p~n",[Other]),
	    loop1(Port)
    end.

loop2(Port, F) ->
    receive
	{From, getFun} ->
	    From ! {?MODULE, F},
	    loop1(Port)
    end.

goto(X,Y) -> [$\e,$[,integer_to_list(X),$;,integer_to_list(Y),$H].
clear_screen() -> "\e[2J".


for(I, I, F) -> F(I);
for(I, Max, F) -> F(I),for(I+1,Max,F).

%% The telnet server spawns a FUN every time we get a new connection
%% Fun/0 returns a Pid - it then sends character messages to this Pid

start_telnet_server() ->
    {ok, Listen} = gen_tcp:listen(0,
				  [binary,
				   %% {dontroute, true},
				   {nodelay,true},
				   {packet, 0},
				   {reuseaddr, true},
				   {active, true}]),
    {ok, Port} = inet:port(Listen),
    %% io:format("telnet server started on port:~p~n",[Port]),
    spawn_link(fun() -> par_connect(Listen) end),
    Port.

par_connect(Listen) ->
    process_flag(trap_exit, true),
    {ok, Socket} = gen_tcp:accept(Listen),
    %% make another one
    spawn_link(fun() -> par_connect(Listen) end),
    %% When we get here we're off
    process_flag(trap_exit, true),
    %% The handler owns the socket - but it has to spawn
    %% a control process
    Self = self(),
    %% Ask the server for the Fun
    Fun = elib1_misc:rpc(?MODULE, getFun),
    Pid = spawn_link(fun() -> Fun(Self) end),
    self() ! [?IAC, ?WILL, ?ECHO,
	      ?IAC, ?WILL, ?SURPRESS_GO_AHEAD,
	      ?IAC, ?DO, ?NAWS  %% this causes window resize events
	     ],
    driver_loop(Socket, Pid, "", 0).

driver_loop(Socket, Pid, [IAC,?SB,?NAWS,0,X,0,Y,IAC,?SE|T], 0) ->
    Pid ! {resize,X,Y},
    driver_loop(Socket, Pid, T, 0);
driver_loop(Socket, Pid, [255|T], 0) ->
    %% eat the next two characters
    driver_loop(Socket, Pid, T, 2);
driver_loop(Socket, Pid, [H|T], 0) ->
    send(Pid, H),
    driver_loop(Socket, Pid, T, 0);
driver_loop(Socket, Pid, [_|T], N) ->
    driver_loop(Socket, Pid, T, N-1);
driver_loop(Socket, Pid, [], N) ->
    receive
	{tcp, Socket, Bin} ->
	    Str = binary_to_list(Bin),
	    io:format("telnet server received=~w~n",[Str]),
	    driver_loop(Socket, Pid, Str, N);
	{tcp_closed, Socket} ->
	    Pid ! closed;
	close ->
	    gen_tcp:close(Socket);
	Stuff ->
	    gen_tcp:send(Socket, Stuff),
	    driver_loop(Socket, Pid, [], N)
    end.

send(_, 0) ->
    true;
send(Pid, H) ->
    io:format("dispatching ~p~n", [H]),
    Pid ! {char, H}.
