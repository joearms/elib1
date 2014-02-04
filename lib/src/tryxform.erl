%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(tryxform).
-compile(export_all).

test() ->
    L = elib1_xml:parse_file("lorem.chap"),
    elib1_misc:dump("tryx", L),
    Chapter = get_node(L, "chap"),
    HTML = toHTML([Chapter], [], Chapter),
    file:write_file("out.html",[HTML]).


toHTML([{node,"chap",_,_,L}|_], Path, C) ->
    [<<"<?xml version='1.0' ?>
<!DOCTYPE html PUBLIC '-//W3C//DTD XHTML 1.0 Transitional//EN'
    'http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd'>
<html xmlns='http://www.w3.org/1999/xhtml' >
  <head>">>,
     toHTML(L, [chap|Path], C),
     <<"</html>">>];
toHTML([{node,"title",_,_,L}|T], [chap], C) ->
    ["<title>", text(L), "</title>",
     <<"<link href='../../include/elib1.css' rel='stylesheet' />
<script src='../../include/jquery-1.3.2.min.js'
	type='text/javascript'> </script>
</head>
<body>">>,toHTML(T, [chap], C),<<"</body>">>];
toHTML([{node,Tag,_,_,L}|T], Path, C) ->
    ["<",Tag,">",
     toHTML(L, [Tag|Path], C),
     "</",Tag,">"|toHTML(T, Path, C)];
toHTML([{raw,_,_,Str}|T], Path, C) ->
    [quote(Str)|toHTML(T, Path, C)];
toHTML([], _, _) ->
    [].

text([{raw,_,_,Str}]) ->
    quote(Str);
text(X) ->
    io:format("text:~p~n",[X]),
    "aa".

quote(X) ->
    elib1_misc:string2html(X).

get_node([{node,N,_,_,_}=X|_], N) -> X;
get_node([_|T], N) -> get_node(T, N).

