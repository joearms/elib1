%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

%% elib1_misc   Miscellaneous functions
%% Time-stamp: <2010-03-18 09:46:21 ejoearm>

%% @doc This is an extemely simple, zero configuration webkit.
%% Using the webkit you can easily make a small HTTP server
%% running on localhost that is good enough for
%% backendign simple applications. It is by no means a
%% full-blown HTTP server, so it is not intended as a replacement
%% for a "proper" HTTP server, but memely as something that is
%% usful for interfaceing small scale web applicttions to your
%% browser.
%%
%% The webkit acts as a "middle man" and abstracts out details
%% of the real HTTP rpotocol convertting it into a form
%% that is convenient for an Erlang program.
%% 
%% that abstract the protocol in different ways
%% The simplest is
%%  Messages from driver 
%%  {Pid, {get, File, Args}}
%%  {Pid, {post, File, Args, Data}}
%%  {Pid, close}
%%  To the client
%%  {page, FileExtension, DeepList} 
%%  {error, What}
%%  close

-module(elib1_webkit).

-import(lists, [map/2, reverse/1, reverse/2]).

-compile(export_all).

-export([start/2,
	 start_batch_mod_server/1,
	 start_fold_server/3,
	 start_mod_server/2,
	 start_static_server/2,
	 serve_static_file/1,
	 serve_static_file_report_error/1,
	 classify/1, 
	 forever/0,
	 mod_server/4,
	 header/1, 
	 mime/1,
	 pre/1, 
	 get_file/1
	]).

start(Port, Fun) ->
    {ok, Listen} = gen_tcp:listen(Port, 
				  [binary,
				   %% {dontroute, true},
				   {nodelay,true},
				   {packet, 0},
				   {reuseaddr, true}, 
				   {active, true}]),
    io:format("listen port:~p~n",[Port]),
    spawn_link(fun() -> par_connect(Listen, Fun) end).

par_connect(Listen, Fun) ->
    process_flag(trap_exit, true),
    {ok, Socket} = gen_tcp:accept(Listen),
    io:format("got another connection~n"),
    %% make another one
    spawn_link(fun() -> par_connect(Listen, Fun) end),
    %% When we get here we're off
    process_flag(trap_exit, true),
    Where = inet:peername(Socket),
    S = self(),
    io:format("http driver opened socket:~p~n",[Socket]),
    Pid = spawn_link(fun() -> Fun(S, Where) end),
    relay(Socket, Pid, {header, []}).

websocket(Socket, Server, State) ->
    receive
	{tcp, Socket, Bin} ->
	    Data = binary_to_list(Bin),
	    %% io:format("<-- ~s~n", [Data]),
	    parse_websocket_request(State, Socket, Server, Data);
	{tcp_closed, Socket} ->
	    io:format("http driver got tcp closed (socket:~p) -" 
		      " so browser closed connection~n",
		      [Socket]),
	    Server ! {self(), closed};
	{Server, close} ->
	    io:format("server closed connection~n"),
	    gen_tcp:close(Socket);
	{raw, Data} ->
	    io:format("sending:~p~n",[Data]),
	    gen_tcp:send(Socket,  [0,Data,255]),
	    websocket(Socket, Server, State);
	{'EXIT', Server, Why} ->
	    io:format("elib1_webkit:relay Server exit reason-~p~n",[Why]),
	    gen_tcp:close(Socket);
	Other ->
	    io:format("http_driver unexpected message:~p~n",[Other]),
	    websocket(Socket, Server, State)
    end.

parse_websocket_request({more, S}, Socket, Server, Data) ->
    S1 = Data ++ S,
    parse1(Data ++ S, Socket, Server).

parse1(S, Socket, Server) ->
    io:format("parse1 S=~p~n",[S]),
    case get_frame(S) of
	{yes, Frame, Rest} ->
	    io:format("Frame=~p~n",[Frame]),
	    Server ! {self(), {websocketData, Frame}},
	    parse1(Rest, Socket, Server);
	more ->
	    websocket(Socket, Server, {more, S})
    end.

get_frame([0|T]) -> get_frame(T, []);
get_frame([]) -> more.
    
get_frame([255|T], L) ->
    {yes, reverse(L), T};
get_frame([H|T], L) ->
    get_frame(T, [H|L]);
get_frame([], L) ->
    more.

relay(Socket, Server, State) ->
    receive
	{tcp, Socket, Bin} ->
	    Data = binary_to_list(Bin),
	    %% io:format("<-- ~s~n", [Data]),
	    parse_request(State, Socket, Server, Data);
	{tcp_closed, Socket} ->
	    io:format("http driver got tcp closed (socket:~p) -" 
		      " so browser closed connection~n",
		      [Socket]),
	    Server ! {self(), closed};
	{Server, close} ->
	    io:format("server closed connection~n"),
	    gen_tcp:close(Socket);
	{websocket, Data} ->
	    io:format("sending:~p~n",[Data]),
	    gen_tcp:send(Socket,  Data),
	    websocket(Socket, Server, {more, []});
	{response, Tag, Data} ->
	    B1  = list_to_binary(Data),
	    Len = size(B1),
	    Mime = mime_type(Tag),
	    Packet = ["HTTP/1.1 200 Ok\r\n", content_type(Mime),
		      "Content-Length: ", integer_to_list(Len), "\r\n\r\n",
		      B1],
	    %% io:format("Packet=~p~n",[B1]),
	    gen_tcp:send(Socket, Packet),
	    relay(Socket, Server, State);
	{response1,Code,Headers,Data} ->
	    Headers1 = [[Key,":",Val,"\r\n"] || {Key, Val} <- Headers],
	    Bin = list_to_binary(Data),
	    CL = ["Content-Length:",i2s(size(Bin)),"\r\n\r\n"],
	    Packet = ["HTTP/1.1", i2s(Code), " Ok\r\n", Headers1, CL, Bin],
	    gen_tcp:send(Socket, Packet),
	    relay(Socket, Server, State);
	{error, Code} ->
	    Packet = ["HTTP/1.1 ",i2s(Code),
		      " Error\r\nContent-Length:0\r\n\r\n"],
	    %% io:format("Packet=~p~n",[Packet]),
	    gen_tcp:send(Socket, Packet),
	    relay(Socket, Server, State);
	{Server, {Headers, Data}} ->
	    %% io:format("--> ~p ~p~n", [Headers1, B1]),
    	    gen_tcp:send(Socket, [Headers,Data]),
	    relay(Socket, Server, State);
	{'EXIT', Server, Why} ->
	    io:format("elib1_webkit:relay Server exit reason-~p~n",[Why]),
	    gen_tcp:close(Socket);
	Other ->
	    io:format("http_driver unexpected message:~p~n",[Other]),
	    relay(Socket, Server, State)
    end.

parse_request({header, Buff}, Socket, Server, Data) ->
    case scan_header(Data, Buff) of
	{no, Buff1} ->
	    %% not enought data to parse the header
	    relay(Socket, Server, {header, Buff1});
	{yes, Header, After} ->
	    %% we've now got enought data to parse the header
	    got_header(Socket, Server, Header, After)
    end;
parse_request({post, Buff, Len, X}, Socket, Server, Data) ->
    case collect_chunk(Len, Data, Buff) of
        {yes,PostData,After} ->
            Args2 = parse_uri_args(PostData),
	    {Op,Vsn,URI,Args1,Header} = X,
	    Request = {Op,Vsn,URI,Args1 ++ Args2,Header},
            Server ! {self(), Request},
	    parse_request({header,[]}, Socket, Server, After);
        {no,Buff1,Len1} ->
            State = {post, Buff1, Len1, X},
	    relay(Socket, Server, State)
    end.

got_header(Socket, Server, Header, After) ->
    %% We've got the header - parse it
    %% io:format("Header=~p~n",[Header]),
    case parse_header(Header) of
	{0, Result} ->
	    %% Send the parsed request to the server
	    %% io:format("**finally sending:~p~n",[{self(),Result}]),
	    Server ! {self(), Result},
	    %% go get the next request
	    parse_request({header,[]}, Socket, Server, After);
	{ContentLen, Result} ->
	    %% only post commands have extra data
	    State = {post, [], ContentLen, Result},
	    parse_request(State, Socket, Server, After)
    end.

collect_chunk(0, New, Buf)      -> {yes, reverse(Buf), New};
collect_chunk(N, [H|T], Buff)   -> collect_chunk(N-1, T, [H|Buff]);
collect_chunk(N, [], Buff)      -> {no, Buff, N}.

%% scan_header is a reentrant scanner that collects data up to
%% \r\n\r\n

scan_header([$\n|T], [$\r,$\n,$\r|L]) -> {yes, reverse(L), T};
scan_header([H|T],  L)                -> scan_header(T, [H|L]);
scan_header([], L)                    -> {no, L}.
				
mime_type(gif)  ->  "image/gif";
mime_type(jpg) -> "image/jpeg";
mime_type(png) -> "image/png";
mime_type(css)  -> "text/css";
mime_type(json)  -> "application/json";
mime_type(swf)  -> "application/x-shockwave-flash";
mime_type(html) -> "text/html";
mime_type(xul) -> "application/vnd.mozilla.xul+xml";
mime_type(js)   -> "application/x-javascript";
mime_type(svg)   -> "image/svg+xml";
mime_type(X) when is_atom(X) -> mime_type(html);
mime_type(FileName) -> mime_type(classify(FileName)).

mime(Tag) ->
    content_type(mime_type(Tag)).

classify(FileName) ->
    case string:to_lower(filename:extension(FileName)) of
	".gif"  -> gif;
	".jpg"  -> jpg;
	".jpeg" -> jpg;
	".css"  -> css;
	".js"   -> js;
	".svg"   -> svg;
	".xul"  -> xul;
	".html" -> html;
	".htm"  -> html;
	_       -> html
    end.

header(X) when is_atom(X) -> 
    ["HTTP/1.0 200 Ok\r\n", powered_by(), content_type(mime_type(X))];
header({redirect,To}) ->
    ["HTTP/1.0 302 Come and get it!\r\n",
     powered_by(), "Location: " ++ To ++ "\r\n"].

powered_by() ->
    "X-Powered-By: Erlang \r\n".

content_type(X) ->
    ["Content-Type: ", X, "\r\n"].

%% parse_header(Str) -> {ContentLength, {Verb,Vsn, URI, Args, Headers}\\
%%   Verb = get | put\\
%%   ContentLen = the length of any additional data that has to be 
%%                fetched

parse_header(Str) ->
    %% {ok, Fields} = regexp:split(Str, "\r\n"),
    Fields = re:split(Str, "\r\n",[{return,list}]),
    {Verb, Vsn, Path, Args} = parse_request(hd(Fields)),
    %% Args = "KeyWord: Str" ..
    Headers  = map(fun isolate_arg/1, tl(Fields)),
    Len = content_length(Headers),
    {Len, {Verb, Vsn, Path, Args, Headers}}.

%% I've lower cased to header keys
%% so Content-Length is lower cased here

content_length([{"content-length",Str}|_]) -> list_to_integer(Str);
content_length([_|T]) ->  content_length(T);
content_length([])    ->  0.

urlencoded2str([$%,Hi,Lo|T]) -> [decode_hex(Hi, Lo)|urlencoded2str(T)];
urlencoded2str([$+|T])       -> [$ |urlencoded2str(T)];
urlencoded2str([H|T])        -> [H|urlencoded2str(T)];
urlencoded2str([])           -> [].

isolate_arg(Str) -> isolate_arg(Str, []).

isolate_arg([$:,$ |T], L) -> {string:to_lower(reverse(L)), T};
isolate_arg([H|T], L)     -> isolate_arg(T, [H|L]).

%% decode_hex ...

decode_hex(Hex1, Hex2) ->
    hex2dec(Hex1)*16 + hex2dec(Hex2).

hex2dec(X) when X >=$0, X =<$9 -> X-$0;
hex2dec($A) -> 10;
hex2dec($B) -> 11;
hex2dec($C) -> 12;
hex2dec($D) -> 13;
hex2dec($E) -> 14;
hex2dec($F) -> 15;
hex2dec($a) -> 10;
hex2dec($b) -> 11;
hex2dec($c) -> 12;
hex2dec($d) -> 13;
hex2dec($e) -> 14;
hex2dec($f) -> 15.

%% parse_request(Str) -> {Verb,Vsn,Path,Args}
%%   parse first line of an HTTP response

parse_request(Str) ->
    %% {ok, Fields} = regexp:split(Str, " "),
    Fields = re:split(Str, " ",[{return,list}]),
    case Fields of
	["POST", URI, Vsn] ->
	    {Path, Args} = parse_uri(URI),
	    {post, parse_vsn(Vsn) , Path, Args};
	["GET", URI, Vsn] ->
	    {Path, Args} = parse_uri(URI),
	    {get, parse_vsn(Vsn), Path, Args};
	_  ->
	    exit({badRequest,Str})
    end.

parse_vsn("HTTP/1.0") -> {1,0};
parse_vsn(X) -> X.

%% A typical URI looks
%% like
%% URI = "/a/b/c?password=aaa&invisible=Ahidden+value"+

parse_uri(URI) ->
    case string:tokens(URI, "?") of
	[Root] ->
	    {Root, []};
	[Root, Args] ->
	    {Root, parse_uri_args(Args)}
    end.

parse_uri_args(Args) ->
    Args1 = string:tokens(Args, "&;"),
    map(fun(KeyVal) ->
	       case string:tokens(KeyVal, "=") of
		   [Key, Val] ->
		       {urlencoded2str(Key), urlencoded2str(Val)};
		   [Key] ->
		       {urlencoded2str(Key), ""};
		   _ ->
		       io:format("Invalid str:~p~n",[KeyVal]),
		       {"error", "error"}
	       end
       end, Args1).

i2s(I) ->
    integer_to_list(I).

get_file(File) ->
    case file:read_file("." ++ File) of
	{ok, Bin} ->
	    Type = classify(File),
	    {ok, Type, Bin};
	Error ->
	    Error
    end.

pre(X) ->
    ["<pre>\n",quote(lists:flatten(io_lib:format("~p",[X]))), "</pre>"].

quote("<" ++ T) -> "&lt;" ++ quote(T);
quote("&" ++ T) -> "&amp;" ++ quote(T);
quote([H|T]) -> [H|quote(T)];
quote([]) -> [].
    
forever() ->
    receive
	after infinity ->
		true
	end.

%% Now for some specialised servers
%% Fun4(get|put, Uri, Args, State) ->
%%    {response,Type,Data,State'}
%%    {resoponseH, Type, Data,State'}
%%    {erroer,Code,State'}

start_fold_server(Port, Fun4, State) ->
    %% From should be local host ... but I don't check
    start(Port, fun(MM, _From) ->  loop4(MM, Fun4, State) end).
    
loop4(MM, Fun4, State) ->
    receive
	{MM, {Tag, _Vsn, Uri, Args, _Headers}} ->
	    %% io:format("MM:~p ~p ~p~n",[Tag,Uri,Args]),
	    case Fun4(Tag, Uri, Args, State) of
		{response, Type, Data, State1} ->
		    MM ! {response, Type, Data},
		    loop4(MM, Fun4, State1);
		{responseH, Type, Headers, Data, State1} ->
		    MM ! {response1, Type, Headers, Data},
		    loop4(MM, Fun4, State1);
		{error, Code, State1} ->
		    MM ! {error, Code},
		    loop4(MM, Fun4, State1);
		_ ->
		    io:format("elib1_webkit:Bad Fun=~p ~p~n",[Uri,Args]),
		    loop4(MM, Fun4, State)
	    end;
	
	Other ->
	    io:format("Message dropped:~p~n",[Other]),
	    loop4(MM, Fun4, State)
    end.



%%----------------------------------------------------------------------
%% start_static_server(Port, Fun3)  a static server.
%% By static is meant a server that is stateless. Each time a request comes
%% Fun3(Tag, Uri, Args) is evaluated. This must return:
%% {response, Type, Data} Type = html,js,gif, ...
%% {error, Code}          Code = 400, ...
%% {response1, Type, Headers, Data}


start_static_server(Port, Fun3) ->
    %% From should be local host ... but I don't check
    start(Port, fun(MM, _From) ->  loop2(MM, Fun3) end).
    
loop2(MM, Fun) ->
    receive
	{MM, {Tag, _Vsn, Uri, Args, _Headers}} ->
	    %% io:format("MM:~p ~p ~p~n",[Tag,Uri,Args]),
	    MM ! Fun(Tag, Uri, Args),
	    loop2(MM, Fun);
	{MM, {websocketData, Data}} ->
	    MM ! Fun(get, "$socket", Data),
	    loop2(MM, Fun);
	Other ->
	    io:format("Message dropped loop2:~p~n",[Other]),
	    loop2(MM, Fun)
    end.


serve_static_file(File) ->
    case file:read_file(File) of
	{ok, Bin}  -> {response, classify(File), [Bin]};
	{error, _} -> {error, 400}
    end.

serve_static_file_report_error(File) ->
    case file:read_file(File) of
	{ok, Bin}  -> {response, classify(File), [Bin]};
	{error, _} -> io:format("** missing file:~p~n",[File]),
		      {response, html, pre({missing,file,File})}
    end.

%% start_mod_server(Port, RootDir)
%%   example:
%%   start_mod_server(2009, "/home/joe/here/we/are").
%%   This starts a server on Port
%%   With root directory RoorDir
%%   GET requests to files are assumed to be relative toe RootDir
%%   If the path is the form /mod/Mod/Func?Args
%%   Then Mod:Func(Args) will be called
%%   Mod must be located in RootDir and will be recompiled if necessary

start_batch_mod_server([P,D]) ->
    Port = list_to_integer(atom_to_list(P)),
    Dir = atom_to_list(D),
    start_mod_server(Port, Dir),
    forever().

start_mod_server(Port, RootDir) ->
    start_static_server(Port, fun(Tag, Uri, Args) ->
				      mod_server(Tag, Uri, Args, 
						 filename:split(RootDir))
			      end).

mod_server(Tag, Uri, Args, Root) ->
    %% io:format("Starting server Root=~p~n",[Root]),
    io:format("elib1_webkit mod_server URI=~p~n",[Uri]),
    Parts = filename:split(Uri),
    case Parts of
	["/","mod",Mod,Func] ->
	    exec_mod(Mod,Func,Args,Root);
	["/"|F] ->
	    %% io:format("elib1_webkit mod_server F=~p~n",[F]),
	    Full = filename:join(Root ++ F),
	    %% io:format("Serve file:~p~n",[Full]),
	    serve_static_file(Full);
	_Other ->
	    {response, html, [pre({mod_server,Parts,Tag,Uri,Args,Root})]}
    end.

%% Rules:
%%   The beam code must be loaded from Root
%%   The erlang module must be in Root
%%   The Erlang module is compiled if out of date 

exec_mod(SMod, Func, Args, Root) ->
    Mod = list_to_atom(SMod),
    case code:is_loaded(Mod) of
	false ->
	    compile_then_eval(SMod, Func, Args, Root);
	{file, FullName} ->
	    %% The beam code was loaded
	    Dir = filename:dirname(FullName),
	    %% Dir is the directory where we found the code
	    %% check that it is the same as root
	    case filename:join(Root) of
		Dir ->
		    %% ok .. beam in correct directory;
		    compile_then_eval(SMod, Func, Args, Root);
		Dir1 ->
		    error({object,code,in,wrong,dir,
			   fullname,FullName,
			   is,Dir,should,be,Dir1})
	    end
    end.

compile_then_eval(SMod, SFunc, Args, Root) ->
    case recompile(SMod, Root) of
	ok ->
	    Func = list_to_atom(SFunc),
	    Mod = list_to_atom(SMod),
	    apply(Mod, Func, [Args, filename:join(Root)]);
	Other ->
	    Other
    end.

recompile(Mod, Root) ->
    Stem = filename:join(Root ++ [Mod]),
    Src = Stem ++ ".erl",
    Beam = Stem ++ ".beam",
    case {filelib:last_modified(Src),
	  filelib:last_modified(Beam)} of
	{0, _} ->
	    error({no,src,Src});
	{T1, T2} when T1 > T2 -> 
	    recompile(Src);
	_ ->
	    ok
    end.

recompile(Src) ->
    io:format("** recompiling:~p~n",[Src]),
    case compile:file(Src, [report]) of
	{ok, Mod} ->
	    code:purge(Mod),
	    code:load_file(Mod),
	    ok;
	{error, E, W} ->
	    error({errors,E,warnings,W})
    end.

error(X) ->    
    {response, html, [pre({error, X})]}.

    


