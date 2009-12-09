%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(midi_driver).
-compile(export_all).

%% this is not right yet. If I have two AUs loaded
%% then I always get the Pianoteq trial ...
%% something is wrong with the driver

test1() ->
    spawn(fun() ->
		  Keyboard = connect_keyboard("USB Axiom 25 Port 1"),
		  Synt = connect_synt("ARIA Player"),
		  %% Synt = connect_synt("Pianoteq Trial 3"),
		  echo(Keyboard, Synt)
	  end).

echo(K, S) ->
    receive
	{midi, M} ->
	    io:format("M=~w~n",[M]),
	    S ! {play, M},
	    echo(K, S);
	Any ->
	    io:format("Any-=~p~n",[Any]),
	    echo(K, S)
    end.

test2() ->
    spawn(fun() ->
		  Keyboard = connect_keyboard("USB Axiom 25 Port 1"),
		  collect_events(20)
	  end).

collect_events(0) ->
    true;
collect_events(N) ->
    receive
	Any ->
	    io:format("received:~p~n",[Any]),
	    collect_events(N-1)
    end.


test3() ->
    Synt = connect_synt("ARIA Player"),
    play(Synt, 60),
    close(Synt).

play(Pid, 80) ->
    true;
play(Pid, N) ->
    Pid ! {play, [144,N,118]},
    sleep(200),
    Pid ! {play, [128,N,118]},
    play(Pid, N+1).

close(Pid) ->
    Pid ! stop.


connect_keyboard(Name) ->
    {Dest, _} = devices(),
    case [I || {I, N} <- Dest, N =:= Name] of
	[Idev] ->
	    Pid = midi_in_driver:start(Idev),
	    io:format("~s started~n",[Name]),
	    Pid;
	_ ->
	    io:format("There is no dest called:~sChoose between:~n~p~n",
		      [Name, Dest]),
	    exit(eNoSynt)
    end.


%%----------------------------------------------------------------------
%% connect_synt(Name)

connect_synt(Name) ->
    {Dests, _} = devices(),
    case [I || {I, N} <- Dests, N =:= Name] of
	[Idev] ->
	    Pid = midi_out_driver:start(Idev),
	    io:format("device:~p ~s started~n",[Idev, Name]),
	    Pid;
	_ ->
	    io:format("There is no synthesiser called:~s~nChoose between:~n~p~n",
		      [Name, Dests]),
	    exit(eNoSynt)
    end.

devices() ->
    Str = os:cmd(elib1_misc:root_dir() ++ "/examples/midi_driver/list_devices"),
    io:format("Str=~p~n",[Str]),
    L = elib1_misc:string2value(Str),
    Dest = [{I,S} || {dest,I,S} <- L],
    Device = [{I,S} || {device,I,S} <- L],
    Ret = {Dest,Device},
    io:format("devices=~p~n",[Ret]),
    Ret.


sleep(T) ->
    receive
	after T ->
		true
	end.


		  

