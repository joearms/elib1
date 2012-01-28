%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(irc_text_client).

-compile(export_all).

local() ->
    User = "j" ++ elib1_misc:random_string(5),
    Nick = "j" ++ elib1_misc:random_string(5),
    start("localhost", User, Nick).

%% start() ->    start("irc.freenode.net").
%% irc_text_client:start("localhost", "joe", "armstrong").
%% irc_text_client:start("localhost", "mike", "williams").

start(Host, Nick, User) ->
    S = self(),
    Pid = spawn_link(fun() -> start(S, Host) end),
    receive
	{Pid, ok} ->
	    %% Pid ! {send, "USER " ++ User ++ " myhost localhost :snark\r\n"},
	    Pid ! {send, "USER " ++ User ++ "\r\n"},
	    Pid ! {send, "NICK " ++ Nick ++ "\r\n"},
	    cmd_loop(Pid);
	{Pid, Error} ->
	    Error
    end.

cmd_loop(Pid) ->
    case io:get_line(' > ') of
	"quit\n" ->
	    exit(Pid);
	Str ->
	    Str1 = remove_nl(Str),
	    io:format("sending ~s~n",[Str1]),
	    Pid ! {send, Str1 ++ "\r\n"},
	    cmd_loop(Pid)
    end.

remove_nl("\n") -> [];
remove_nl([H|T]) -> [H|remove_nl(T)].


start(Parent, Host) ->
    io:format("trying to connect to:~p port 6669~n",[Host]),
    case gen_tcp:connect(Host, 6669, [{packet,0}], 60000) of
	{ok, Socket} ->
	    io:format("connected~n"),
	    Parent ! {self(), ok},
	    loop(Socket);
	Error ->
	    Parent ! {self(), Error}
    end.

loop(Socket) ->
    receive
	{send, Str} ->
	    io:format(" >> ~p~n", [Str]),
	    gen_tcp:send(Socket, Str),
	    loop(Socket);
	{tcp, Socket, Str} ->
	    io:format(" << ~s", [Str]),
	    case Str of
		"PING " ++ T ->
		    self() ! {send, "PONG " ++ T};
		_Other ->
		    void
	    end,
	    loop(Socket);
	{tcp_closed, Socket} ->
	    io:format("socket closed~n"),
	    exit(bye);
	Other ->
	    io:format("unexpected:~p~n",[Other]),
	    loop(Socket)
    end.

