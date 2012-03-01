%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(midi_in_driver).

%% Midi_in_driver

-compile(export_all).

test() ->
    io:format("Port process starting~n"),
    start(6).

start(Idev) -> 
    S = self(),
    Pid = spawn(fun() -> run(S, Idev) end),
    receive
	{Pid, ack} ->
	    Pid
    end.


run(Parent, Idev) ->
    process_flag(trap_exit, true),
    Prog = elib1_misc:root_dir() ++ "/examples/midi_driver/midi_in_driver",
    Port = open_port({spawn, Prog}, [{packet, 2}]),
    Port ! {self(), {command, [Idev]}},
    wait_ready(Port),
    Parent ! {self(), ack},
    loop(Port, Parent).

wait_ready(Port) ->
    receive
	{Port, {data, "ready"}} ->
	    true;
	Any ->
	    io:format("mid_in_driver unexpected:=~p~n",[Any]),
	    wait_ready(Port)
    end.


stop(Pid) ->
    Pid ! stop.

loop(Port, Pid) ->
    receive
	stop ->
	    Port ! {self(), close};
	{send, Msg} ->
	    Port ! {self(), {command, Msg}},
	    loop(Port, Pid);
	{Port, {data, D}} ->
	    Pid ! {midi, D},
	    loop(Port, Pid);
	Any ->
	    io:format("dropping:~p~n",[Any]),
	    loop(Port, Pid)
    end.

