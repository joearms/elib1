%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(ircd).
%%%-------------------------------------------------------------------
%%% File    : ircd.erl
%%% Author  : joe armstrong <joe@localhost>
%%% Description :
%%%
%%% Created :  8 Dec 2009 by joe armstrong <joe@localhost>
%%%-------------------------------------------------------------------

%% testing three windows ...
%% ircd:start_server()
%% irc_tex

-import(lists, [reverse/1]).

-behaviour(gen_server).

-compile(export_all).

%% API
-export([start_register/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([register_name/2, find_or_create_group/1]).

start_register() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register_name(Name, Pid) ->
    gen_server:call(?MODULE, {register,Name,Pid}).

-spec find_or_create_group(Name::string()) -> pid().

%% always succeeds

find_or_create_group(Name) ->
    gen_server:call(?MODULE, {findorc,Name}).

init([]) ->
    process_flag(trap_exit, true),
    {ok, ets:new(foo, [set,private])}.

handle_call({findorc, "#" ++ _ = Name}, _From, Ets) ->
    %% always works
    case ets:lookup(Ets, Name) of
	[] ->
	    %% Need to create a group process
	    Pid = spawn_link(fun() -> group_loop([]) end),
	    ets:insert(Ets, [{Name,Pid},{Pid,Name}]),
	    {reply, Pid, Ets};
	[{_,Pid}] ->
	    {reply, Pid, Ets}
    end;
handle_call({register, Name, Pid}, _From, Ets) ->
    case ets:lookup(Ets, Name) of
	[] ->
	    link(Pid),
	    ets:insert(Ets, [{Name,Pid},{Pid,Name}]),
	    {reply, true, Ets};
	_ ->
	    {reply, false, Ets}
    end.

handle_cast({forward, Name, Msg}, Ets) ->
    case ets:lookup(Ets, Name) of
	[]        -> void;
	[{_,Pid}] -> Pid ! Msg
    end,
    {noreply, Ets}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    io:format("INFO~p~n",[Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

start_server() ->
    io:format("ircd starting~n"),
    spawn(fun() -> start_server0() end).

start_server0() ->
    start_register(),
    process_flag(trap_exit, true),
    Self = self(),
    {ok, Listen} = gen_tcp:listen(6669, [{packet,0},
					 {reuseaddr,true},
					 {active, true}]),
    spawn_link(fun() -> par_connect(Listen, Self) end),
    loop(Listen).

loop(Listen) ->
    receive
	connect ->
	    Self = self(),
	    spawn_link(fun() -> par_connect(Listen, Self) end),
	    loop(Listen)
    end.

par_connect(Listen, Parent) ->
    io:format("waiting for connection~n"),
    {ok, Socket} = gen_tcp:accept(Listen),
    {ok, {IP, _}} = inet:peername(Socket),
    io:format("got a connection~n"),
    Parent ! connect,
    Self = self(),
    Pid = spawn_link(fun() -> idle(Self,IP,"ident") end),
    client_loop(Socket, Pid, []).

client_loop(Socket, Pid, Buff) ->
    receive
	{tcp, Socket, Data} ->
	    io:format("<< ~p~n",[Data]),
	    Buff1 = Buff ++ Data,
	    handle_line(Socket, Pid, Buff1);
	{tcp_close, Socket} ->
	    exit(i);
	{msg, M} ->
	    L = format_reply(M),
	    gen_tcp:send(Socket, L),
	    client_loop(Socket, Pid, Buff);
	{send, Str} ->
	    gen_tcp:send(Socket, Str),
	    client_loop(Socket, Pid, Buff);
	Any ->
	    io:format("client_loop dropped:~p~n",[Any]),
	    client_loop(Socket, Pid, Buff)
    end.

handle_line(Socket, Pid, Buff) ->
    io:format("handle ~p~n",[Buff]),
    case get_line(Buff, []) of
	more ->
	    client_loop(Socket, Pid, Buff);
	{Line, Buff1} ->
	    Pid ! {cmd, parse_line(Line)},
	    handle_line(Socket, Pid, Buff1)
    end.

get_line([$\r,$\n|T], L) -> {reverse(L), T};
get_line([$\n|T], L)     -> {reverse(L), T};
get_line([H|T], L)       -> get_line (T, [H|L]);
get_line([], _)          -> more.


parse_line(L) ->
    {Head, T}  = get_body(L, []),
    {string:tokens(Head, " "), T}.

get_body(" :" ++ T, L) -> {reverse(L), T};
get_body([H|T], L)     -> get_body(T, [H|L]);
get_body([], L)        -> {reverse(L), []}.

%%----------------------------------------------------------------------

host() ->
    "irc.erlang.org".

format_reply({1, Nick}) ->
    [":", host()," 001 ", Nick, " :Welcome to the Internet Relay Network ",
     Nick,"\r\n"];
format_reply({2, Nick}) ->
    [":",host()," 002 ", Nick,
     " :Your host is ", host(), ",running version 0\r\n"];
format_reply({3, Nick}) ->
    [":",host()," 003 ", Nick,
     ": This server was created Wed Dec  9 13:32:17 2009\r\n"];
format_reply({4,Nick}) ->
    %% this is a lie :-)
    [":",host()," 004 ", Nick, " ", host()," 0 dioswkg biklmnopstv\r\n"];
format_reply({mod,Nick}) ->
   [":", host()," 375 ", Nick, ":- ", host(), " Message of the Day \n",
    ":",  host()," 372 ", Nick, ":- ", host(),
    " If you can't do it in Erlang, you don't want to do it.\r\n",
    ":", host()," 376 ", Nick, ":End of /MOTD command.\r\n"];

format_reply({433, ShortName}) ->
    [":", host()," 433 *", ShortName, " :Nickname already in use\r\n"].

%% Each user is handled by a state machine
%% The state is
%% idle -> USER -> NICK -> running

idle(MM,IP,User) ->
    receive
	{cmd, {["USER",Who|_], _}} ->
	    %% USER Name :....
	    idle(MM, IP, Who);
	{cmd, {["NICK", Nick|_], _}} ->
	    case register_name(Nick, self()) of
		true ->
		    %% it worked send a whole bundle of messages
		    MM ! {msg, {1, Nick}},
		    MM ! {msg, {2, Nick}},
		    MM ! {msg, {3, Nick}},
		    MM ! {msg, {4, Nick}},
		    MM ! {msg, {mod, Nick}},
		    running(MM, Nick, []);
		false ->
		    MM ! {msg, {433, Nick}},
		    idle(MM, IP, User)
	    end;
	Other ->
	    io:format("idle Other=~p~n",[Other]),
	    idle(MM, IP, User)
    end.

running(MM, Nick, L) ->
    receive
	{cmd, {["JOIN", Group], _}} ->
	    case Group of
		"#" ++ _ ->
		    Pid = find_or_create_group(Group),
		    Pid ! {join, Nick, self()},
		    running(MM, Nick, [Group|L]);
		_->
		    MM ! {send, "Bad group name\r\n"},
		    running(MM, Nick, L)
	    end;
	{sendClient, Msg} ->
	    MM ! {sendClient1, {Nick, Msg}},
	    running(MM, Nick, L);
	Other ->
	    io:format("running Other=~p~n",[Other]),
	    running(MM, Nick, L)
    end.

%% group_loop(Id)
%%    L = [{Nick,Pid}]
%%    who are members of the group

group_loop(Id) ->
    process_flag(trap_exit, true),
    group_loop(Id, []).

group_loop(Id, L) ->
    receive
	{join, Name, Pid} ->
	    case lists:keysearch(Name, 1, L) of
		false ->
		    L1 = [{Name,Pid}|L],
		    broadcast(Id, L1, {joined, Id, Name}),
		    group_loop(Id, L1);
		_ ->
		    %% ignore
		    group_loop(Id, L)
	    end;
	{leave, Name} ->
	    case lists:keysearch(Name, 1, L) of
		false ->
		    group_loop(Id, L);
		{value, Val} ->
		    broadcast(Id, L, {left, Name}),
		    L1 = lists:delete(Val, L),
		    group_loop(Id, L1)
	    end;
	{broadcast, Msg} ->
	    broadcast(Id, L, Msg),
	    group_loop(Id, L);
	Other ->
	    io:format("group:~s unexpected message:~p~n",[Id, Other]),
	    group_loop(Id, L)
    end.

broadcast(Id, [{Name,Pid}|T], Msg) ->
    io:format("Group ~s sending ~p to ~s~n",[Id,Msg,Name]),
    Pid ! {sendClient, Msg},
    broadcast(Id, T, Msg);
broadcast(_,  [], _) ->
    void.
