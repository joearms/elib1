%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(simple_web_server).

-compile(export_all).
-import(elib1_webkit, [pre/1]).
-import(lists, [sort/1]).

%% START:start
start() ->
    elib1_webkit:start_static_server(2246, fun server/3),
    elib1_misc:forever().
%% END:start

server(_,"/cgi/start_irc", _) ->
    spawn(fun() -> chat_widget:test1() end),
    {response,html,"<h1>Irc running in another window</h1>"};
server(_,"/cgi/elib1_webquery", Args) ->
    elib1_webquery:qu(Args, elib1_misc:root_dir() ++ "/supported/indexer");
server(Tag, Uri, Args) ->
    io:format("Uri=~p~n",[Uri]),
    Root = elib1_misc:root_dir(),
    FullName = Root ++ Uri,
    %% if the file extension is .ehtml then call the expander
    case filename:extension(Uri) of
	".ehtml" ->
	    case filelib:is_file(FullName) of
		true ->
		    Str = elib1_expand:expand_file(FullName),
		    {response, html, [Str]};
		false ->
		    {response, html, pre({missing,file,FullName})}
	    end;
	_ ->
	    %% just serve the file
	    elib1_webkit:serve_static_file_report_error(FullName)
    end.

%% the header uses jquery to add some standard stuff to the html body

header() ->
    ["<link  href='/include/elib1.css' type='text/css' rel='stylesheet'/>\n",
     "<script language='JavaScript'
        SRC='/include/jquery-1.3.2.min.js'></script>
<script>
$(document).ready(function(){
   $('body').prepend('<img width=\"100\" src=\"/include/images/ring.gif\"><img width=\"200\" src=\"/include/images/ringtxt.jpg\">');
});
</script>"].

supported() ->
    Dir = elib1_misc:root_dir() ++ "/supported",
    V = elib1_misc:ls(Dir),
    Ds = [D || {D,dir,_} <- V] -- [".svn"],
    ["<ul>", [mk_link(I) || I <- sort(Ds)], "</ul>"].

mk_link(L) ->
    ["<li><a href='/supported/",L,"/doc.ehtml'>",L,"</a></li>"].

unsupported() ->
    Dir = elib1_misc:root_dir() ++ "/unsupported",
    V = elib1_misc:ls(Dir),
    Ds = [D || {D,dir,_} <- V] -- [".svn"],
    ["<ul>", [mk_link1(I) || I <- sort(Ds)], "</ul>"].

mk_link1(L) ->
    ["<li><a href='/unsupported/",L,"/doc.ehtml'>",L,"</a></li>"].

library() ->
    Root =  elib1_misc:root_dir(),
    Dir = Root ++ "/lib/src",
    L = elib1_find:files(Dir,"*.erl",false),
    L1 = sort([filename:basename(filename:rootname(I)) || I <- L]),
    ["<ul>", [mk_link1(I, Root) || I <- L1], "</ul>"].

mk_link1(I, Root) ->
    L1 = ["<a href='",
	  "/supported/tagger/html/",I,".html'>",I,"</a>"],
    L2 = ["<a href='",
	  "/lib/doc/",I,".html'>edoc</a>"],
    ["<li>", L1, " ", L2, "</li>"].

notes() ->
    Root =  elib1_misc:root_dir(),
    Dir = Root ++ "/wanted/notes",
    L = elib1_find:files(Dir,"*.ehtml",false),
    io:format("L=~p~n",[L]),
    L1 = sort([filename:basename(filename:rootname(I)) || I <- L]),
    ["<ul>", [mk_link3(I) || I <- L1], "</ul>"].

mk_link3(I) ->
    L1 = ["<a href='",
	  "/wanted/notes/",I,".ehtml'>",I,"</a>"],
    ["<li>", L1,"</li>"].



