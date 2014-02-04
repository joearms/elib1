%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(elib1_doc).

%% -compile(export_all).
-export([batch/1, file/1, file/2, setup/1]).

%% takes a file like this <html> ... </html>
%% strips the header and replaces with a valid xhml header
%% expands <e>...</e>

-import(lists, [map/2, reverse/1, reverse/2]).

batch([X]) ->
    File = filename:rootname(atom_to_list(X)),
    file(File).

%% file converts F.ehtml to F.html in the same directory

file(F) ->
    io:format("elib1_doc::~s~n",[F]),
    file(F ++ ".ehtml", F ++ ".html").

file(InFile, OutFile) ->
    case file:read_file(InFile) of
	{ok, Bin} ->
	    Str1 = binary_to_list(Bin),
	    Str2 = remove_top_level_markup(Str1),
	    Str3 = elib1_expand:expand_string(Str2),
	    Str4 = add_xhtml_markup(InFile, Str3),
	    file:write_file(OutFile, Str4);
	_ ->
	    cannot_read_file
    end.

remove_top_level_markup("<html>" ++ T)     -> remove_top_level_markup(T, []).

remove_top_level_markup("</html>" ++ _, L) -> reverse(L);
remove_top_level_markup([H|T], L) -> remove_top_level_markup(T, [H|L]).

add_xhtml_markup(File, L) ->
    Root = filename:rootname(filename:basename(File)),
    [<<"<!DOCTYPE html PUBLIC '-//W3C//DTD XHTML 1.0 Strict//EN'
	'http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd'>
	<html xmlns='http://www.w3.org/1999/xhtml'>\n">>,
     setup(Root),
     L,
     <<"</body></html>\n">>].

setup(File) ->
    ["<head>
	<title>", File, "</title>
        <link href='../include/elib1.css' type='text/css' rel='stylesheet'/>
      </head>
      <body>
	<a href='/cgi?mod=elib1_content_edit&func=edit&file=",
     File,"'>edit</a>

	"].
