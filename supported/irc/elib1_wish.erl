%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(elib1_wish).

%% Time-stamp: <2009-12-07 21:49:17 joe>
%% Copyright (C) 2005 by Joe Armstrong
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.

-compile(export_all).

-export([start/0,
	 stop/0,
	 new_index/0,
	 status/0,
	 cast/1,
	 cmd/1,
	 do/1,
	 make_window/2,
	 controlling_process/3,
	 %% register_window/2,
	 %% install_dir/0,
	 rpc/2]).

-import(lists, [map/2, member/2, flatten/1, foldl/3, foreach/2,
		reverse/1, reverse/2]).

-define(EXIT(X,Y),io:format("** EXIT ~p Module:~p Line:~p Args:~p~n",
			     [X,?MODULE,?LINE,Y]),
		   exit(X)).

%% Interface
%%   start(Host, Port) -> {ok, Pid} | {error, Why}
%%   stop(Pid)              -- stop server
%%   mkWindow(Pid)          -- rpc in server
%%   incastPid, Args)        -- send command to server

start() ->
    elib1_misc:ensure_started(wish83, fun() -> open_graphics() end).

-spec make_window(Owner::pid(), Title::string()) -> Window::string().

make_window(Owner, Title) ->
    Name = ".w" ++ integer_to_list(new_index()),
    register_window(Name, Owner),
    cast(["toplevel ", Name]),
    cast(["bind ", Name, " <Destroy> {windowDestroyed %W }"]),
    cast(["wm title ", Name, " {", Title, "}"]),
    Name.

cast(A) ->
    internal_cast(quoteXX(lists:flatten(A))).

cmd(A) ->
    internal_cmd(quoteXX(lists:flatten(A))).

quoteXX(L) -> [$",quoteXX1(L),$"].

quoteXX1([H|T]) when H==${; H==$}; H==$[;H==$];H==$$;H==$" ->
    [$\\,H|quoteXX1(T)];
quoteXX1([H|T]) -> [H|quoteXX1(T)];
quoteXX1([]) -> [].

open_graphics() ->
    %% This process must continue as the server
    %% We must also start a driver
    Self = self(),
    process_flag(trap_exit, true),
    Driver = spawn_link(fun() -> start_driver(Self) end),
    receive
	{Driver, ok} ->
	    case (catch server_loop(Driver, 1, [], 1)) of
		{'EXIT', Why} ->
		    io:format("*** server loop crashed:~p~n",
			      [Why]);
		stopped ->
		    exit(normal)
	    end;
	{'EXIT', Driver, What} ->
	    io:format("wish driver error:~p~n", [What]),
	    exit(wish_driver)
    end.

%% strategy
%% 1) I create a listening socket
%% 2) I spawn Wish and tell wish the listening socket
%% 3) Wish opens the socket to me
%% 4) If erlang dies the socket is closed -- this is trapped by
%%    Wish and wish then exits

%% Problems - I might choose a "well-known port"

%% start_driver sends a {self(), ok | error} message
%% to it's parent once it has started

start_driver(Parent) ->
    process_flag(trap_exit, true),
    Dir = install_dir(),
    SocketOpts =
	[
	 {nodelay, true},
	 {packet,4},
	 binary,
	 {reuseaddr,true}
	],
    %% Let OS pick a number
    {ok, ListenSocket} = gen_tcp:listen(0, SocketOpts),
    {ok, ListenPort} = inet:port(ListenSocket),
    io:format("Listening port=~p~n",[ListenPort]),
    Cmd = wish_version() ++ " " ++ Dir ++ "/elib1_wish.tcl " ++
	integer_to_list(ListenPort),
    io:format("Cmd=~p~n",[Cmd]),
    %% Spawn a parallel process which starts wish --
    %% this will open a socket to us
    spawn(fun() -> os:cmd(Cmd) end),
    case gen_tcp:accept(ListenSocket, 10000) of
	{ok, Socket} ->
	    ok =  gen_tcp:controlling_process(Socket, self()),
	    case wait_for_connect_from_wish(Socket) of
		ok ->
		    io:format("rock and roll~n"),
		    Parent ! {self(), ok},
		    case (catch driver_loop(Parent, Socket, [])) of
			{'EXIT', Why} ->
			    io:format("*** driver_loop crashed:~p~n",
				      [Why])
		    end;
		error ->
		    Parent ! {self(), error}
	    end;
	Other ->
	    io:format("Other=~p~n",[Other]),
	    Parent ! {self(), error}
    end.

wish_version() ->
    %% windows C:/Tcl/bin/wish.exe"
    "wish8.5".

wait_for_connect_from_wish(Socket) ->
    receive
	{tcp, Socket, <<"magic sync">>} ->
	    io:format("starting~n"),
	    ok;
	Other ->
	    io:format("uugh:~p~n",[Other]),
	    error
    after 4000 ->
	    error
    end.

new_index() -> rpc1(new_index).

internal_cmd(C) -> rpc1({cmd, C}).

stop()     -> rpc1(stop).

status() -> rpc1(status).

internal_cast(Cmd) -> wish83 ! {cast, Cmd}.

register_window(Win, Pid) ->
    rpc1({register_window, Win, Pid}).

rpc1(X) ->
    wish83 ! {self(), X},
    receive
	{wish83, Reply} ->
	    Reply
    end.
%% L= [{Win,Pid}]

-define(REPLY(X), From ! {wish83, X}).

%% Mon = [{Name,Pid,Ref}]

server_loop(Driver, Free, Mon, N) ->
    %% io:format("server_loop (~p) DRiver=~p E=~p~n", [self(), Driver, E]),
    receive
	{From, {register_window, Win, Pid}} ->
	    Ref = erlang:monitor(process, From),
	    ?REPLY(ack),
	    server_loop(Driver, Free+1, [{Win, Pid, Ref}|Mon], N);
	{sendWin, Win, Event} ->
	    io:format("sendWin:: Win=~p Event =~p~n",[Win,Event]),
	    %% This should be a registered window
	    case [P || {Name,P,_} <- Mon, Win == Name] of
		[Pid] ->
		    io:format("sending ~p to ~p~n",[{event,Event}, Pid]),
		    Pid ! {event, Event};
		_ ->
		    io:format("Registered = ~p~n",[Mon]),
		    io:format("dropping event:~p ~p~n",[Win,Event])
	    end,
	    server_loop(Driver, Free, Mon, N);
	{From, status} ->
	    io:format("Free=~p~n",[Free]),
	    LinkedTo = process_info(self(), links),
	    io:format("Server is linked to:~p~n",[LinkedTo]),
	    io:format("Mon=~p~n",[Mon]),
	    ?REPLY(ack),
	    server_loop(Driver, Free, Mon, N);
	{From, stop} ->
	    ?REPLY(stopped),
	    stopped;
	{From, {cmd, C}} ->
	    %% io:format("wish: cmd ~s~n",[lists:flatten(C)]),
	    %% io:format("~s~n",[lists:flatten(C)]),
	    Reply = wish_cmd(N, Driver, C),
	    ?REPLY(Reply),
	    server_loop(Driver, Free, Mon, N+1);
	{cast, C} ->
	    %% io:format("wish: cast ~s~n",[lists:flatten(C)]),
	    wish_cast(N, Driver, C),
	    server_loop(Driver, Free, Mon, N+1);
	{From, new_index} ->
	    ?REPLY(Free+1),
	    server_loop(Driver, Free+1, Mon, N);
	{'DOWN', _Ref, process, Pid, Why} ->
	    Mon1 = possibly_remove_monitor(Pid, Mon),
	    tell_user_about_error(Pid, Why),
	    server_loop(Driver, Free, Mon1, N);
	{winDestroyed, Win} ->
	    %% We get cascade events
	    %% Win should really be a name like .w1
	    %% But we will also see events like .w1.a .w1.a.b
	    %% For all sub-widgets -- I don't know dow to turn this
	    %% Off .. So Instead I'll just count dots
	    NDots = length([X || X <- Win, X =:= $.]),
	    if
		NDots =:= 1 ->
		    case keysplit(Win, 1, Mon) of
			{found, {_Name, Pid, Ref}, Mon1} ->
			    V = (catch erlang:demonitor(Ref)),
			    %% io:format("de-register=~p~n",[V]),
			    %% kill the process
			    %% if it trapping exit it will trap this
			    exit(Pid, window_died),
			    %% io:format("Mon=~p~nMon1=~p~n",[Mon,Mon1]),
			    server_loop(Driver, Free, Mon1, N);
			missing ->
			    %% these messages seem to cascade I know not why
			    io:format("funny window destroy dropped:~p~n",[Win]),
			    server_loop(Driver, Free, Mon, N)
			end;
		true ->
		    server_loop(Driver, Free, Mon, N)
		end;
	{event, Win, Str} ->
	    %% This comes from wish
	    case lists:keysearch(Win, 1, Mon) of
		{value, {_Index, _Name, Pid, _Ref}} ->
		    Pid ! {event, Str};
		false ->
		    warning("funny~n")
	    end,
	    server_loop(Driver, Free, Mon, N);
	Other ->
	    io:format("****Server Loop Other=~p~n",[Other]),
	    server_loop(Driver, Free, Mon, N)
    end.

possibly_remove_monitor(Pid, [{Win,Pid,Ref}|T]) ->
    (catch erlang:demonitor(Ref)),
    cast(["destroy ",Win]),
    T;
possibly_remove_monitor(Pid, [H|T]) ->
    [H|possibly_remove_monitor(Pid, T)];
possibly_remove_monitor(_, []) ->
    [].

keysplit(Key, Pos, L) -> keysplit(Key, Pos, L, []).

keysplit(Key, Pos, [H|T], L) ->
    case element(Pos, H) of
	Key ->
	    {found, H, reverse(L, T)};
	_ ->
	    keysplit(Key, Pos, T, [H|L])
    end;
keysplit(_Key, _Pos, [], _L) ->
    missing.

warning(X) ->
    io:format("*** warning:~p~n",[X]).


%% tell_user_about_error
%%   Tells the user why a window died ...
%%   very useful when debugging

tell_user_about_error(_,normal) ->
    true;
tell_user_about_error(Pid, Reason) ->
    spawn(fun() -> tell_error(Pid, Reason) end).

tell_error(Pid, Reason) ->
    Win = make_window(self(),"Crash report"),
    Label = Win ++ ".l",
    Str = lists:flatten(io_lib:format("Window:~p~nPid:~p~nTerminated with~n"
				      "---------------~n~p",
				      [Win,Pid,Reason])),
    cast(["label ",Label, " -text {",Str,"}"]),
    cast(["pack ",Label]),
    wait_terminate().

wait_terminate() ->
     receive
	 Any ->
	     io:format("got:~p~n",[Any]),
	     wait_terminate()
     end.

wish_cast(N, Wish, Cmd) ->
    Wish ! {cast, N, Cmd}.

wish_cmd(N, Wish, Cmd) ->
    %% io:format("cmd:(~p) |~s| ~n",[N, lists:flatten(Cmd)]),
    %% Cmd1 = quote_cmd(lists:flatten(Cmd)),
    Wish ! {cmd, N, Cmd},
    receive
	{eret, K, S} ->
	    %% io:format("Cmd:~w ~p~n~p~n",[K,lists:flatten(Cmd),S]),
	    %% io:format("wish83:here1 ~p~n",[lists:flatten(Cmd)]),
	    if
		K == N ->
		    S;
		true ->
		    io:format("*** sync error sent ~w received ~w~n",
			      [N, K])
	    end,
	    S;
	{error, K, E} ->
	    S1 = lists:flatten(Cmd),
	    io:format("*error in cmd:~p~n~p~n",[S1,E]),
	    if
		K == N ->
		    true;
		true ->
		    io:format("*** sync error sent ~w received ~w~n",
			      [N, K])
	    end,
	    error
    after 25000 ->
	    io:format("****** LONGCmd:~w ~p~n",[N,lists:flatten(Cmd)])
	  end.

%% driver_loop talks to the
%% Socket

driver_loop(Server, Socket, Data) ->
    %% io:format("Driver loop(Sever=~p self()=~p Socket=~p Data=~p~n)~n",
    %% [Server, self(), Socket,Data]),
    receive
	{cast, N, C} ->
	    %% C1 = quote(C),
	    Str1 = lists:flatten(["ecast ",integer_to_list(N)," ",C]),
	    %% io:format("wish83:driver_loop cast:~s~n",[Str1]),
	    send_socket(Socket, Str1),
	    %% sleep(100),
	    driver_loop(Server, Socket, Data);
	{cmd, N, C} ->
	    C1 = ["ecall ",integer_to_list(N)," ", C],
	    %% io:format("wish83:driver_loop cmd:~w ~s~n",
	    %% [N, lists:flatten(C1)]),
	    send_socket(Socket, C1),
	    %% sleep(100),
	    driver_loop(Server, Socket, Data);
	{tcp, Socket, Bin} ->
	    %% io:format("received 111 :~p~n",[Bin]),
	    D = binary_to_list(Bin),
	    %% io:format("<<:~p~n",[D]),
	    D1 = Data ++ D,
	    parse_messages(D1, Server),
	    driver_loop(Server, Socket, []);
	{'EXIT', Server, Why} ->
	    io:format("wish_server exit from server:~p~n", [Why]),
	    Socket ! {self(), close};
	X ->
	    io:format("wish_server ** EXIT **~p~n",[X]),
	    ?EXIT(driver_loop, X)
    end.

parse_messages(Data, Server) ->
    io:format("parse_msg:~p~n", [Data]),
    case (catch parse_msg(Data)) of
	{'EXIT', Why} ->
	    io:format("elib1_wish:error paring message:~p why=~p~n",[Data,Why]),
	    void;
	Event ->
	    io:format("Dispatch:~p~n",[Event]),
	    Server ! Event
    end.

-spec parse_event(string()) -> {Win::string(), {SubWin::string(), Data::string()}}.

%% Bs .w1.w2.w3 XXXX => {".w1",".w2.w3",XXX}

parse_event(Str) ->
    [$.|S1] = skip_blanks(Str),
    {Win, S2} = collect_win(S1, [$.]),
    case S2 of
	[$.|_] ->
	    {SubWin, S3} = collect_unil_space(S2, []),
	    S4 = skip_blanks(S3),
	    {Win, {SubWin, S4}};
	_ ->
	    S4 = skip_blanks(S2),
	    {Win, {[], S4}}
    end.

collect_win([$\s|T], L) -> {reverse(L), T};
collect_win([$.|_]=T, L)  -> {reverse(L), T};
collect_win([H|T], L)   -> collect_win(T, [H|L]);
collect_win([], L)      -> {reverse(L), []}.

collect_unil_space([$\s|T], L) -> {reverse(L), T};
collect_unil_space([], L)      -> {reverse(L), []};
collect_unil_space([H|T], L)   -> collect_unil_space(T, [H|L]).

parse_string("{" ++ T) ->
    case reverse(T) of
	"}" ++ T1 -> reverse(T1);
	_ ->
	    io:format("*** ??? an error ???~p~n",[T]),
	    "****"
    end;
parse_string(X) ->
    unquote(X).

unquote([$\\,H|T]) -> [H|unquote(T)];
unquote([H|T]) -> [H|unquote(T)];
unquote([]) -> [].

parse_msg("event " ++ T) ->
    {Win, Args} = parse_event(T),
    {sendWin, Win, Args};
parse_msg("winDestroyed " ++ T) ->
    {winDestroyed, T};
parse_msg("eret " ++ T) ->
    %% io:format("eret=~p~n",[T]),
    {N, T1}    = get_int(T),
    {Str, _T2} = get_string(T1),
    {eret, N, Str};
parse_msg("error " ++ T) ->
    {N, T1}    = get_int(T),
    {Str, _T2} = get_string(T1),
    {error, N, Str};
parse_msg(S=[_H|_T]) ->
    io:format("**** cannot parse:|~p|~n", [S]),
    exit(bad).

get_int(S) -> get_int(skip_blanks(S), 0).

get_int([H|T], N) when $0 =< H, H =< $9 ->  get_int(T, N*10 + H - $0 + 0);
get_int(Other, N)                       ->  {N, Other}.

skip_blanks([$ |T]) -> skip_blanks(T);
skip_blanks(X)      -> X.


get_string(S) -> get_string(skip_blanks(S), []).

% get_string([$\n|T], L) -> {reverse(L), [$\n |T]};

get_string([195,191|T], L) -> {reverse(L), T};
get_string([255|T], L) -> {reverse(L), T};
get_string([H|T], L)   -> get_string(T, [H|L]);
get_string([], L)      -> {reverse(L), []}.



send_socket(Socket, Cmd) ->
    %% S = lists:flatten(Cmd),
    %% io:format("send port |~s|~n", [S]),
    gen_tcp:send(Socket, Cmd).

rpc(Pid, Q) ->
    %io:format("In RPC Pid=~p Q=~p~n",[Pid,Q]),
    Pid ! {self(), Q},
    receive
	{Pid, Reply} ->
	    Reply
    end.

install_dir() ->
    filename:dirname(code:which(?MODULE)).

controlling_process(Pid, Tag, Win) ->
    wish_manager:add_monitor(Pid, Tag, Win).

%% do

do(L) -> cmd(cvt(L)).

cvt([H|T]) when is_list(H)    -> [$\s,H,$\s|cvt(T)];
cvt([H|T]) when is_integer(H) -> [i2s(H),$\s|cvt(T)];
cvt([])                       -> [].

i2s(I) -> integer_to_list(I).

