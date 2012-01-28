%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(web_server_two).

%% callback module for a web server
-compile(export_all).

start() ->
    Root = elib1_misc:root_dir(),
    File = Root ++ "/data.dets",
    elib1_seq_web_server1:start(?MODULE,
				elib1_simple_kv_db,
				[{name,?MODULE},{file,File}]
			       ).

server_port()     -> 2246.
max_connections() -> 200.
root()            ->  elib1_misc:root_dir().

%% handle(Request, Db) -> Response
%%   Request = {get|put, Vsn, Url, Args, Env}
%%   DB = db handler function
%%   DB({read,K}) | DB({write,key,val})
%% Response = {ok, Headers, Body} | {error, What} | close
%% Headers = Atom (html,jpg,...) or [{Key,Val}]

handle({_,_,"/cgi",[{"mod",M},{"func",Str}|Args],_}, F) ->
    Mod = list_to_atom(M),
    Func = list_to_atom(Str),
    Mod:Func(Args, F);
handle({_, Vsn, F0, Args, _Env}, F) ->
    %% DB is a function that can be used to
    %% read of write the DB
    %% DB({read, Key})
    %% DB({write,Key,Val})
    %% Pid
    %% Module_name:read(Pid, Key)
    File = F(root) ++ F0,
    %% io:format("Get:~p~n",[File]),
    elib1_seq_web_server1:default_response(File, Args).

