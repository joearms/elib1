%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(midi_out_driver).

%% Midi_out_driver

-compile(export_all).

test() ->
    io:format("Port process starting~n"),
    start(1).

%% K = number of the midi device to open

start(K) ->
    S = self(),
    Pid = spawn(fun() -> run(S) end),
    receive
	{Pid, ack} ->
	    Pid ! {play,[K]},
	    wait_ready(Pid),
	    Pid
    end.

wait_ready(Pid) ->
    receive
	{synt, "ready"} ->
	    true;
	Any ->
	    io:format("sync Any=~p~n",[Any]),
	    wait_ready(Pid)
    end.

run(Parent) ->
    process_flag(trap_exit, true),
    Prog = elib1_misc:root_dir() ++ "/examples/midi_driver/midi_out_driver",
    Port = open_port({spawn, Prog}, [{packet, 2}]),
    Parent ! {self(), ack},
    loop(Port, Parent).

stop(Pid) ->
    Pid ! stop.

loop(Port, Pid) ->
    receive
	stop ->
	    Port ! {self(), close},
	    receive
		{'EXIT', Port, normal} ->
		    void;
		Any ->
		    io:format("unexpect port message:~p~n",[Any])
	    end;
	{play, Data} ->
	    Port ! {self(), {command, Data}},
	    loop(Port, Pid);
	{Port, {data, D}} ->
	    Pid ! {synt, D},
	    loop(Port, Pid);
	Any ->
	    io:format("dropping:~p~n",[Any]),
	    loop(Port, Pid)
    end.

parse_midi(D) ->
    {parse_midi, D}.

