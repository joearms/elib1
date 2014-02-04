%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(elib1_ml9_2_html).

-export([exec/2, get_data/2]).
-import(lists, [map/2, member/2]).

exec(File, Parse) ->
    B = filename:rootname(File),
    Parse1 = map(fun(I) -> xform(I, Parse) end, Parse),
    file:write_file(B ++ ".html", [css(),Parse1]).

css() ->
    ["<link rel='STYLESHEET' type='text/css' href='base.css'>",
     "<div class=contents>"].

xform({header,X}, _) ->
    Txt = get_data(X),
    ["<h1>",Txt,"</h1>"];
xform({addindex,_}, P) ->
    L = [X||{index,X}<-P],
    Entries = map(fun(I) ->
			  Text = get_data(I),
			  Tag = erlang:md5(Text),
			  ["<li><a href=\"#",Tag,"\">", Text,"</a>"]
		  end, L),
    ["<ul>",Entries,"</ul>"];
xform({index, X}, _) ->
    Txt = get_data(X),
    Tag = erlang:md5(Txt),
    ["<a name=\"",Tag,"\"></a><h1>",Txt,"</h1>"];
xform({para, X}, _) ->
    Txt = get_data(X),
    Paras   = elib1_ml9:parse_para(Txt),
    %% io:format("Paras=~p~n",[Paras]),
    map(fun para_to_html/1, Paras);
xform({pre,X}, _) ->
    Str = get_data(X),
    Color = get_data(X, color, white),
    case Color of
	white ->
	    ["<ul><pre><b>",Str,"</b></pre></ul>"];
	C ->
	    color(C, ["<ul><pre><b>",Str,"</b></pre></ul>"])
    end;
xform(Other, _) ->
    io:format("ignoring:~p~n",[Other]),
    [].

color(C, X) ->
    ["<table><tr><td bgcolor=\"",atom_to_list(C),"\">",
     X,
     "</td></tr></table>\n"].

get_data(Assoc, Key, Default) ->
    case elib1_ml9:search(Key, Assoc) of
	{ok, Val} -> Val;
	error -> Default
    end.


get_data(X) ->
    {_,Str} = elib1_ml9:fetch(data, X),
    Str.

get_data(X, Default) ->
    case elib1_ml9:fetch(data, X) of
	{ok, Val} ->
	    Val;
	error ->
	    Default
    end.

para_to_html(X) ->
    ["<p>",map(fun inline_to_html/1, X)].

inline_to_html({str,S}) -> S;
inline_to_html({italic, S}) -> ["<i>",S,"</i>"];
inline_to_html({bold,S}) -> ["<b>",S,"</b>"];
inline_to_html({code,S}) -> ["<tt>",S,"</tt>"];
inline_to_html({link,Str}) ->
    case member($|, Str) of
	true ->
	    case  string:tokens(Str,"|") of
		[Href,Text] ->
		    ["<a href=\"",Href,"\">",Text,"</a>"];
		_ ->
		    ["<a href=\"",Str,"\">",Str,"</a>"]
	    end;
	false ->
	    Ext = filename:extension(Str),
	    case member(Ext, [".gif",".jpg",".GIF",".JPG",".png"]) of
		true ->
		    ["<img src=\"", Str, "\">"];
		false ->
		    ["<a href=\"",Str,"\">",Str,"</a>"]
	    end
    end;
inline_to_html(C) -> ["<pre>",io_lib:format("unknown inline:~p",[C]),
		     "<pre>"].


