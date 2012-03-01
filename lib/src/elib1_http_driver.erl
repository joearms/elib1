%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(elib1_http_driver).

-import(lists, [map/2, reverse/1, reverse/2]).

-export([start/2, classify_file/1]).
-export([header/1, parse_header/1, urlencoded2str/1]).

%% start(Port, Pid)
%%    starts an http driver
%%    As new http request come they are parsed
%%    and send as messges of the form
%%      Pid ! {self(), request, request()}
%%    to Pid.
%%    where -type request()::{get|post,Uri,Args}

start(Port, ServerPid) ->
    spawn(fun() -> server(Port, ServerPid) end).

server(Port, Server) ->   
    process_flag(trap_exit, true),
    io:format("Starting a port server on ~p...~n",[Port]),
    {ok, Listen} = gen_tcp:listen(Port, [binary,
					 %% {dontroute, true},
					 {nodelay,true},
					 {packet, 0},
					 {reuseaddr, true}, 
					 {active, true}]),
    io:format("got a listening socket well done~n"),
    spawn(fun() -> par_connect(Listen, Server) end),
    elib1_misc:forever().


par_connect(Listen, Server) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    spawn(fun() -> par_connect(Listen, Server) end),
    %% Tell the server we have started a new session 
    Where = inet:peername(Socket),
    Server ! {self(), start, Where},
    relay(Socket, Server, {header, []}).

relay(Socket, Server, State) ->
    receive
	{tcp, Socket, Bin} ->
	    Data = binary_to_list(Bin),
	    %% io:format("<-- ~s~n", [Data]),
	    parse_request(State, Socket, Server, Data);
	{tcp_closed, Socket} ->
	    Server ! {self(), closed};
	{Server, close} ->
	    gen_tcp:close(Socket);
	{Server, {Headers, Data}} ->
	    B1 = list_to_binary(Data),
	    Len = size(B1),
	    Headers1 = Headers ++ ["Content-Length: ",integer_to_list(Len),
				   "\r\n"] 
		++ ["\r\n"],
	    %% io:format("--> ~p ~p~n", [Headers1, B1]),
    	    gen_tcp:send(Socket, [Headers1, B1]),
	    relay(Socket, Server, State);
	{'EXIT', Server, _} ->
	    gen_tcp:close(Socket)
    end.

parse_request({header, Buff}, Socket, Server, Data) ->
    case scan_header(Data, Buff) of
	{no, Buff1} ->
	    relay(Socket, Server, {header, Buff1});
	{yes, Header, After} ->
	    got_header(Socket, Server, Header, After)
    end;
parse_request({post, Buff, Len, X}, Socket, Server, Data) ->
    case collect_chunk(Len, Data, Buff) of
        {yes,PostData,After} ->
            Args2 = parse_uri_args(PostData),
	    {Op,Vsn,URI,Args1,Env} = X,
	    Request = {Op,Vsn,URI,Args1++Args2,Env},
            Server ! {self(), request, Request},
	    parse_request({header,[]}, Socket, Server, After);
        {no,Buff1, Len1} ->
            State = {post, Buff1, Len1, X},
	    relay(Socket, Server, State)
    end.

got_header(Socket, Server, Header, After) ->
    %% We've got the header - parse it
    case parse_header(Header) of
	_Result = {Op, ContentLen, Vsn, URI, Args, Env} ->
	    case ContentLen of
		0 ->
		    %% Send the parsed request to the server
		    Msg =  {Op,Vsn,URI,Args,Env},
		    %% io:format("sending reqquest:~p~n",[Msg]),
		    Server ! {self(), request, Msg},
		    %% go get the next request
		    parse_request({header,[]}, Socket, Server, After);
		_ ->
		    State = {post, [], ContentLen, {Op,Vsn,URI,Args,Env}},
		    parse_request(State, Socket, Server, After)
		end;
	Other ->
	    io:format("Oops ~p ~n", [Other]),
	    exit(debug)
    end.

collect_chunk(0,New,Buf)      -> {yes, reverse(Buf), New};
collect_chunk(N, [H|T], Buff) -> collect_chunk(N-1,T,[H|Buff]);
collect_chunk(N, [], Buff)    -> {no, Buff, N}.

scan_header([$\n|T], [$\r,$\n,$\r|L]) -> {yes, reverse(L), T};
scan_header([H|T],  L)                -> scan_header(T, [H|L]);
scan_header([], L)                    -> {no, L}.
				
%%----------------------------------------------------------------------

header(X) when is_atom(X) -> 
    ["HTTP/1.0 200 Ok\r\n", powered_by(), content_type(mime_type(X))];
header({redirect,To}) ->
    ["HTTP/1.0 302 Come and get it!\r\n",
     powered_by(), "Location: " ++ To ++ "\r\n"].

powered_by() ->
    "X-Powered-By: Erlang \r\n".

content_type(X) ->
    ["Content-Type: ", X, "\r\n"].

%%----------------------------------------------------------------------

parse_header(Str) ->
    Fields = split_lines(Str),
    PRequest = parse_request(hd(Fields)),
    %% Args = "KeyWord: Str" ..
    PArgs = map(fun isolate_arg/1, tl(Fields)),
    make_return_value({PRequest, PArgs}).

split_lines([]) -> [];
split_lines(Str) ->
    {Line, Str1} = collect_line(Str, []),
    [Line|split_lines(Str1)].

collect_line("\r\n" ++ T, L) -> {reverse(L), T};
collect_line([H|T], L)       -> collect_line(T, [H|L]);
collect_line([], L)          -> {reverse(L), []}.


make_return_value({{Op,Vsn,{URI,Args}}, Env}) ->
    {Op, content_length(Env), Vsn, URI, Args, Env}.


content_length([{"content-length",Str}|_]) -> list_to_integer(Str);
content_length([_|T])                      -> content_length(T);
content_length([])                         -> 0.

urlencoded2str([$%,Hi,Lo|T]) -> [decode_hex(Hi, Lo)|urlencoded2str(T)];
urlencoded2str([$+|T])       -> [$ |urlencoded2str(T)];
urlencoded2str([H|T])        -> [H|urlencoded2str(T)];
urlencoded2str([])           -> [].


isolate_arg(Str) -> isolate_arg(Str, []).

isolate_arg([$:,$ |T], L) -> {elib1_misc:to_lower(reverse(L)), T};
isolate_arg([H|T], L)     -> isolate_arg(T, [H|L]).

%% decode_hex %%

decode_hex(Hex1, Hex2) ->
    hex2dec(Hex1)*16 + hex2dec(Hex2).

hex2dec(X) -> elib1_misc:hex_nibble2int(X).

parse_request(Str) ->
    Args = split(Str),
    case Args of
	["POST", URI, Vsn] ->
	    {post, parse_vsn(Vsn) ,parse_uri(URI)};
	["GET", URI, Vsn] ->
	    {get, parse_vsn(Vsn), parse_uri(URI)};
	_  -> 
	    oops
    end.

split([$\s|T]) -> split(T);
split([]) -> [];
split(Str) ->
    {Item, Str1} = collect_until_space(Str, []),
    [Item|split(Str1)].

collect_until_space([$\s|T], L) -> {reverse(L), T};
collect_until_space([], L)      -> {reverse(L), []};
collect_until_space([H|T], L)   -> collect_until_space(T, [H|L]).

parse_vsn("HTTP/1.0") -> {1,0};
parse_vsn(X) -> X.

%% A typical URI looks
%% like
%% URI = "/a/b/c?password=aaa&invisible=A+hidden+value"

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

%% some useful things surrounding http
%% mime_type(Atom) -> String

mime_type(gif)  -> "image/gif";
mime_type(jpg)  -> "image/jpeg";
mime_type(png)  -> "image/png";
mime_type(css)  -> "text/css";
mime_type(json) -> "application/json";
mime_type(swf)  -> "application/x-shockwave-flash";
mime_type(html) -> "text/html";
mime_type(xul)  -> "application/vnd.mozilla.xul+xml";
mime_type(js)   -> "application/x-javascript";
mime_type(svg)  -> "image/svg+xml";
mime_type(X) when is_atom(X) -> mime_type(html);
mime_type(FileName) -> mime_type(classify_file(FileName)).

%% classify_file(Filename) -> MimeType
%%   try to figure out the mime type depending upon the 
%%   filename

classify_file(FileName) ->
    case elib1_misc:to_lower(filename:extension(FileName)) of
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

