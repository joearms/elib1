%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(example1).
-export([test/0, start/0, stop/0]).
-export([twice/1, sum/2]).

test() ->
    io:format("Port process starting~n"),
    start(),
    case (catch begin
		    4 = twice(2),
		    5 = sum(3,2),
		    ok
		end) of
	{'EXIT', Why} ->
	    io:format("Something wrong:~p~n",[Why]);
	ok ->
	    io:format("test worked~n")
    end,
    stop(),
    io:format("Port process stopped~n"),
    init:stop().

start() -> 
    S = self(),
    Pid = spawn(fun() -> run(S) end),
    receive
	{Pid, ack} ->
	    true
    end.

run(Parent) ->
    register(example1, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, "./port_driver"}, [{packet, 2}]),
    Parent ! {self(), ack},
    loop(Port).

stop() ->
    example1 ! stop.

twice(X) -> call_port({twice, X}).
sum(X,Y) -> call_port({sum, X, Y}).

call_port(Msg) ->
    example1 ! {call, self(), Msg},
    receive
	{example1, Result} ->
	    Result
    end.

loop(Port) ->
    receive
	{call, Caller, Msg} ->
	    Port ! {self(), {command, encode(Msg)}}, 
	    receive
		{Port, {data, Data}} ->
		    Caller ! {example1, decode(Data)}
	    end,
	    loop(Port);
	stop ->
	    Port ! {self(), close},
	    receive
		{Port, closed} ->
		    exit(normal)
	    end;
	{'EXIT', Port, Reason} ->
	    exit({port_terminated,Reason})
    end.

encode({twice, X}) -> [1, X];  
encode({sum, X, Y}) -> [2, X, Y]. 

decode([Int]) -> Int. 
