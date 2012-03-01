-module(elib1_txt2xml).
-compile(export_all).
-import(lists, [reverse/1, reverse/2]).

%% first step of document massage. Break a text file into parahraphs
%% and output as XML

test() ->
    Files = elib1_find:files(".", "*.txt", false),
    [cvt(I) || I <- Files].

cvt(F) ->
    L = elib1_misc:file2paras(F),
    H = ["<?xml version='1.0'?>\n",
	 "<!DOCTYPE chapter SYSTEM 'story.dtd'>\n",
	 "<chapter>\n",[fix(P) || {I, P} <- L],"</chapter>\n"],
    io:format("created:~p~n",[F++".xml"]),
    file:write_file(F ++ ".xml", [H]).

fix(L) ->
    L1 = normalise_ws(L, []),
    L2 = quotify(L1, 0, []),
    L3 = linebreak(L2, [], []),
    L4 = to_para(L3).

to_para(L) ->
    ["<p>\n",
     [["  ",string2html(Str),"\n"] || Str <- L],
     "</p>\n"].


string2html("<author>" ++ T) -> "<author>" ++ string2html(T);
string2html("</author>" ++ T) -> "</author>" ++ string2html(T);
string2html("<" ++ T) -> "&lt;" ++ string2html(T);
string2html([H|T])    -> [H|string2html(T)];
string2html([])       -> [].


linebreak([], [], L) ->
    reverse(L);
linebreak([], This, L) ->
    reverse([reverse(This)|L]);
linebreak(Str, This, L) ->
    {Word, Str1} = get_next_word(Str, []),
    case 1 + length(Word) + length(This) of
	K when K > 75 ->
	    %% start a new line
	    This1 = reverse(Word),
	    linebreak(Str1, This1, [reverse(This)|L]);
	_ ->
	    This1 = reverse(Word, This),
	    linebreak(Str1, This1, L)
    end.

get_next_word([$\s|T], L) ->
    {reverse([$\s|L]), T};
get_next_word([H|T], L) ->
    get_next_word(T, [H|L]);
get_next_word([], L) ->
    {reverse(L), []}.

quotify([$"|T], 0, L) ->
    quotify(T, 1, reverse("&o;", L));
quotify([$"|T], 1, L) ->
    quotify(T, 0, reverse("&c;", L));
quotify([$'|T], N, L) ->
    quotify(T, N, reverse("&s;", L));
quotify([H|T], N, L) ->
    quotify(T, N, [H|L]);
quotify([], 0, L) ->
    reverse(L);
quotify([], 1, L) ->
    L1 = reverse("<author>Quotes?</author>", L),
    reverse(L1).

normalise_ws([], [$\s|L]) ->
    normalise_ws([], L);
normalise_ws([], L) ->
    reverse(L);
normalise_ws([H|T], L) when H =:= $\n; H =:= $\r; H =:= $\t ->
    normalise_ws([$\s|T], L);
normalise_ws([$\s|T], [$\s|_]=L) ->
    normalise_ws(T, L);
normalise_ws([H|T], L) ->
    normalise_ws(T, [H|L]).




