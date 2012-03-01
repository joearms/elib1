%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(elib1_telnet).

-compile(export_all).

%% Time-stamp: <2009-10-16 20:51:30 joe>

-import(lists, [duplicate/2, foreach/2]).
-import(mom_utils, [remove_prefix/2, longest_common_prefix/1]).

-define(IAC, 255). %% Interpret as command
-define(DO, 253).  %% 
-define(WILL, 251).
-define(SB, 250). %% sub negociation Begin
-define(SE, 240). %% Sub negociation End
-define(ECHO, 1).
-define(NAWS, 31).
-define(SURPRESS_GO_AHEAD, 3).
%% \verb+http://en.wikipedia.org/wiki/ANSI_escape_code#Examples+

start() ->
    start(2000, fun handler/2),
    run(),
    elib1_misc:forever().

run() ->
    Cmd = case os:cmd("uname") of
	      "Darwin\n" ->
		  "./term_osx.sh -x telnet localhost 2000"; 
	      _ ->
		  "gnome-terminal -x telnet localhost 2000"
		      end,
    Val = os:cmd(Cmd),
    io:format("Val=~p~n",[Val]).


handler(Pid, Where) ->
    io:format("here1:~p ~p~n",[Pid, Where]),
    Pid ! "\e[32;40;1m",
    Pid ! clear_screen(),
    for(10,20, fun(I)->
		       Pid ! goto(I, I),
		       Pid ! "Wow"
	       end),
    loop(Pid).

goto(X,Y) -> [$\e,$[,integer_to_list(X),$;,integer_to_list(Y),$H].
clear_screen() -> "\e[2J".
    

for(I, I, F) -> F(I);
for(I, Max, F) -> F(I),for(I+1,Max,F).

loop(Pid) ->
    receive
	{char, C} ->
	    Pid ! [C],
	    loop(Pid);
	Any ->
	    io:format("Any=~p~n",[Any]),
            loop(Pid)
    end.

%% The telnet server spawns a FUN every time we get a new connection
%% Fun/0 returns a Pid - it then sends character messages to this Pid

start(Port, Fun) ->
    io:format("Starting a telnet server on Port:~w ~n",
	      [Port]),
    {ok, Listen} = gen_tcp:listen(Port, 
				  [binary,
				   %% {dontroute, true},
				   {nodelay,true},
				   {packet, 0},
				   {reuseaddr, true}, 
				   {active, true}]),
    io:format("listen port:~p~n",[Port]),
    spawn_link(fun() -> par_connect(Listen, Fun) end).

par_connect(Listen, Fun) ->
    process_flag(trap_exit, true),
    {ok, Socket} = gen_tcp:accept(Listen),
    %% make another one
    spawn_link(fun() -> par_connect(Listen, Fun) end),
    %% When we get here we're off
    process_flag(trap_exit, true),
    Where = inet:peername(Socket),
    %% The handler owns the socket - but it has to spawn
    %% a control process
    Self = self(),   
    Pid = spawn_link(fun() -> Fun(Self, Where) end),
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
