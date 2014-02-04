%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(make_html).

-compile(export_all).

start() ->
    process_dir(elib1_misc:root_dir()),
    process_dir(code:lib_dir(stdlib)),
    process_dir(code:lib_dir(kernel)),
    process_dir(code:lib_dir(compiler)).

process_dir(Dir) ->
    L = elib1_find:files(Dir, "*.erl", true),
    lists:foreach(fun mk_html/1, L).

mk_html(In) ->
    F = filename:rootname(filename:basename(In)),
    Out = "./html/" ++ F ++ ".html",
    case elib1_misc:out_of_date(In, Out) of
	true -> elib1_tagger:erl2html(In, Out);
	false -> void
    end.



