%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(complex).
-export([bug/0, test/0, start/0, start/1, stop/0, init/2]).
-export([plus_one/1, times_two/1]).

test() ->
    start(),
    3 = plus_one(2),
    6 = times_two(3),
    io:format("Horray C interface with linked-in driver works~n"),
    stop(),
    init:stop().

bug() ->
    case erl_ddll:load_driver(".", "example_drv") of
        ok -> ok;
        {error, X} -> erl_ddll:format_error(X)
    end.

start() ->
    start("example_drv").

%% erl_ddll:load_driver(".","example_drv").

start(SharedLib) ->
    case erl_ddll:load_driver(".", SharedLib) of
        ok -> ok;
        {error, already_loaded} -> ok;
	{error, X} ->
	    io:format("Error:~p", [erl_ddll:format_error(X)]),
	    exit(eBadStuff)
    end,
    S = self(),
    spawn(?MODULE, init, [S, SharedLib]),
    receive
	{S, ack} ->
	    true
    end.
    

init(P, SharedLib) ->
    register(complex, self()),
    P ! {P, ack},
    Port = open_port({spawn, SharedLib}, []),
    loop(Port).

stop() ->
    complex ! stop.

plus_one(X) ->
    call_port({plus, X}).

times_two(Y) ->
    call_port({times, Y}).

call_port(Msg) ->
    complex ! {call, self(), Msg},
    receive
        {complex, Result} ->
            Result
    end.

loop(Port) ->
    receive
        {call, Caller, Msg} ->
            Port ! {self(), {command, encode(Msg)}},
            receive
                {Port, {data, Data}} ->
                    Caller ! {complex, decode(Data)}
            end,
            loop(Port);
        stop ->
            Port ! {self(), close},
            receive
                {Port, closed} ->
                    exit(normal)
            end;
        {'EXIT', Port, Reason} ->
            io:format("~p ~n", [Reason]),
            exit(port_terminated)
    end.

encode({plus, X}) -> [1, X];
encode({times, Y}) -> [2, Y].

decode([Int]) -> Int.
