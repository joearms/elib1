%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(irc).

-compile(export_all).
-import(lists, [reverse/2, reverse/1, sort/1]).

%% A text mode IRC client

test12() ->
    Str = io:get_line(' ready > '),
    io:format("Str=~p~n",[Str]),
    test12().    

start() ->
    Pid = spawn(fun() -> handler() end),
    read_command(Pid).

%% try LIST
%% JOIN #aaaa

read_command(Pid) ->
    Str = io:get_line(' ready > '),
    io:format("Str=~p~n",[Str]),
    Str1 = elib1_misc:remove_trailing_whitespace(Str),
    io:format("doing ~p ~p~n",[Pid,Str1]),
    Pid ! {send, Str1},
    read_command(Pid).

handler() ->
    S = self(),
    Pid = spawn_link(fun() -> connector(S) end),
    idle(Pid).

-record(irc, {nick = [],     %% My Nick
	      list=[],       %% result of a list command
	      mod = {[],[]}  %% message of the day
	     }).

idle(Pid) ->
    Me = "jim_" ++ elib1_misc:random_string(8),
    receive
	{connected, _Host, _Port} ->
	    send(Pid, ["NICK " ++ Me,
		       "USER " ++ Me ++ " 0 * :my real name is"
		       "peg leg loombucket the third"]),
	    running(Pid, Me, #irc{nick=Me})
    end.

send(Pid, L) ->
    [Pid ! {send, M} || M <- L].

running(Pid, Me, S) ->
    receive
	{notice, Str} ->
	    io:format("~s~n",[Str]),
	    running(Pid, Me, S);
	{[From,"001",Nick], _} ->
	    io:format("My nick is:~p~n",[Nick]),
	    running(Pid, Nick, S#irc{nick=Nick});
	{[From,"375",Me], Str} ->
	    %% MOD start
	    running(Pid, Me, S#irc{mod={[],[]}});
	{[From,"372",Me],Str} ->
	    {A,B} = S#irc.mod,
	    running(Pid, Me, S#irc{mod={A,reverse(Str,[$\n|B])}});
	{[From,"376",Me],Str} ->
	    {_,B} = S#irc.mod,
	    A = reverse(B),
	    io:format("MOD=~n~s~n",[A]),
	    running(Pid, Me, S#irc{mod={A,[]}});
	%% LIST (all)
	%% LIST #erlang
	{[_,"321"|_],_} ->
	    %% liststart
	    running(Pid, Me, S#irc{list=[]});
	{[_,"322",Me,Name,Num],Txt} ->
	    %% list data
	    L = S#irc.list,
	    running(Pid, Me, S#irc{list=[{Name,Num,Txt}|L]});
	{[_,"323",Me],_} ->
	    %% end of data
	    L = S#irc.list,
	    Channels = sort([ {I,N} || {I,N,_} <- L]),
	    io:format("List=~p~n",[Channels]),
	    running(Pid, Me, S);
	{send, Msg} = X ->
	    Pid ! X,
	    running(Pid, Me, S);
	Any ->
	    io:format("domatch Me=~p Msg:~p~n",[Me,Any]),
	    running(Pid, Me, S)
    end.


%% message of the day
%% code 375 = modStart
%% code 372* = mod
%% code 376 = modEnd
    
connector(Parent) ->
    %% Host = "irc.freenode.net", 
    %% Host = "irc.webchat.org",
    Host = "irc.efnet.net",
    Port = 6667,
    io:format("Trying to connect to ~s:~w~n",[Host,Port]),
    case gen_tcp:connect(Host, Port,
			 [{active,true}], 20000) of
	{ok, Socket} ->
	    Parent ! {connected, Host, Port},
	    raw_loop(Parent, Socket, []);
	Other ->
	    io:format("Error:~p~n",[Other])
    end.
   
raw_loop(Parent, Socket, Buff) ->
    receive
	{send, M} ->
	    io:format("sending >> ~p~n",[M]),
	    gen_tcp:send(Socket, [M,"\r\n"]),
	    raw_loop(Parent, Socket, Buff);
	{tcp, Socket, Data} ->
	    %% io:format(">>>~p~n",[Data]),
	    L1 = Buff ++ Data,
	    L2 = process(L1, Parent, Socket),
	    raw_loop(Parent, Socket, L2);
	Any ->
	    io:format("Handler:~p~n",[Any]),
	    raw_loop(Parent, Socket, Buff)
    end.

process(L, Parent, Socket) ->
    case get_line(L, []) of
	{Str, L1} ->
	    case parse_line(Str) of
		{ping, X} ->
		    io:format("pongin~n"),
		    gen_tcp:send(Socket, ["PONG ",X,"\r\n"]),
		    process(L1, Parent, Socket);
		Parse ->
		    Parent ! Parse,
		    process(L1, Parent, Socket)
	    end;
	more ->
	    L
    end.

get_line("\r\n" ++ T, L) -> {reverse(L), T};
get_line([H|T], L)       -> get_line(T, [H|L]);
get_line([], _)          -> more.

parse_line("PING " ++ T) ->
    {ping, T};
parse_line("NOTICE " ++ T) ->
    {notice, T};
parse_line(":" ++ T) ->
    {S1, S2} = split_header(T, []),
    {string:tokens(S1, " "), S2};
parse_line(L) ->
    {unparsed, L}.

split_header(" :" ++ T, L) -> {reverse(L), T};
split_header([H|T], L)     -> split_header(T, [H|L]);
split_header([], L)        -> {[], reverse(L)}.
