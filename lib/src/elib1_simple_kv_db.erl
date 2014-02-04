%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(elib1_simple_kv_db).

-export([start_link/1]).

%% simplest of all KV database using uses dets

%% Interface:
%%    start(Args) -> Pid
%% Protocol
%%    Pid ! {From, {read, key}} => From ! {self(), Ret}
%%        Ret = {ok, V} | error
%%    Pid ! {From, {write, Key, Val}} => From ! {self(), ack}
%%    Pid ! {From, close}           => From ! {self(), ack}

start_link([{name,Name},{file,File}]) ->
    spawn_link(fun() -> run(Name, File) end).

run(Name, File) ->
    Val = dets:open_file(Name, [{auto_save,2000},
				{file,File}]),
    {ok, Name} = Val,
    io:format("Opening:~p name=~p~n",[File, Name]),
    loop(Name).

loop(Name) ->
    receive
	{From, {read, Key}} ->
	    From ! {self(), lookup_i(Name, Key)},
	    loop(Name);
	{From, {write, Key, Val}} ->
	    store_i(Name, Key, Val),
	    From ! {self(), ack},
	    loop(Name);
	{From, info} ->
	    From ! {self(), dets:info(Name)},
	    loop(Name);
	{From, keys} ->
	    %% all keys in the db
	    Vals = dets:foldl(fun({Key,_}, L) -> [Key|L] end, [], Name),
	    From ! {self(), Vals},
	    loop(Name);
	{From, close} ->
	    dets:close(Name),
	    From ! {self(), ack},
	    void;
	Other ->
	    io:format("~p:unexpected message:~p~n",[?MODULE, Other]),
	    loop(Name)
    end.

lookup_i(Name, Key) ->
    Ret = case dets:lookup(Name, Key) of
	      [{_,Val}] -> {ok, Val};
	      _Other     -> error
	  end,
    %% io:format("**real lookup ~p => ~p~n",[Key,Ret]),
    Ret.

store_i(Name, Key, Val) ->
    %% io:format("**real insert:~p ~p~n",[Key,Val]),
    dets:insert(Name, {Key, Val}).



