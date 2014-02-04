%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(elib1_content_edit).

-compile(export_all).
-import(elib1_seq_web_server1, [pre/1]).
-import(lists, [reverse/1, reverse/2]).

edit([{"file", F}], Root, _) ->
    %% F is the name relative to doc
    Full = Root ++ "/doc/" ++ F ++ ".ehtml",
    io:format("elib1_CEDIT Edit File=~p Root=~p Full=~p~n",[F, Root,Full]),
    case file:read_file(Full) of
	{ok, Bin} ->
	    Str = binary_to_list(Bin),
	    Str1 = strip_headers_and_trailers(Str),
	    Bin1 = list_to_binary(Str1),
	    %% io:format("Str=~s~nStr1=~s~n",[Str, Str1]),
	    Template = Root ++ "/doc/edit.html",
	    Args = [{"content", Bin1},{"file",F}],
	    R = elib1_misc:template_file_and_args_to_io_list(Template,
							     Args),
	    {ok, html, [R]};
	{error, _} ->
	    {ok, html, pre({cannot,read,Full})}
    end.

list_dir(A, Root, Db) ->
    io:format("list_dir:~p~n",[{A,Root,Db}]),
    Files = filelib:wildcard(Root ++ "/doc/*.ehtml"),
    L = [["<li><a href='/cgi?mod=elib1_content_edit&func=show_ehtml&file=",
	  I,"'>",I,"</a></li>"] || I <- Files],
    L1 = ["<ul>",L,"</ul>"],
    {ok, html, L1}.

show_ehtml([{"file",F}], _, _) ->
    Src = filename:rootname(F) ++ ".ehtml",
    Dest = filename:rootname(F) ++ ".html",
    case elib1_misc:out_of_date(Src, Dest) of
	true ->
	    elib1_doc:file(filename:rootname(Src));
	false ->
	    void
    end,
    show_html(Dest).

show_html(F) ->
    {ok, B} = file:read_file(F),
    {ok, html, [B]}.


save([{"file", File}, {"value", Str}], Root) ->
    post_process(Root, File, Str),
    {ok, html, pre("done")}.

handle(Args, Root) ->
    io:format("** ERROR cedit:~p~n",[{args,Args,root,Root}]),
    {ok, html, pre({cedit,error,Args,Root})}.

%%----------------------------------------------------------------------
%% post_process

post_process(Root, File, Str) ->
    %% File comes back without the .ehtml extension
    File1 = Root ++ "/doc/" ++ File ++ ".ehtml",
    io:format("Post_process:: ~s ~s~n",[File,File1]),
    Str1 = tidy(Str, [], []),
    Str2 = ["<html>\n",Str1,"</html>\n"],
    make_backup_copy(File1),
    file:write_file(File1, Str2),
    elib1_doc:file(filename:rootname(File1)).

make_backup_copy(Src) ->
    %% Src   is /a/b/c.xxx
    %% Dest     /a/b/backup/c_<time>.xxx
    Dir = filename:dirname(Src),
    File = filename:basename(filename:rootname(Src)),
    Ext = filename:extension(Src),
    Dest = Dir ++ "/backup/" ++ File ++ "_" ++ elib1_misc:time_stamp() ++ Ext,
    io:format("Rename :: ~s as ~s~n",[Src,Dest]),
    file:rename(Src, Dest).

strip_headers_and_trailers("<html>" ++ _ = T) ->
    strip_headers_and_trailers(T, []);
strip_headers_and_trailers([H|T]) ->
    strip_headers_and_trailers(T).

strip_headers_and_trailers("</html>" ++ _, L) ->
    reverse(L);
strip_headers_and_trailers([H|T], L) ->
    strip_headers_and_trailers(T,  [H|L]).

flatten(L) ->
    binary_to_list(list_to_binary(L)).

%% remove strange stuff added by content editable mode
tidy("<span style=\"font-weight: bold;\">" ++ T, S, L) ->
    tidy(T, ["</b>"|S], reverse("<b>", L));
tidy("<span style=\"text-decoration: line-through;\">" ++ T, S, L) ->
    tidy(T, ["</strike>"|S], reverse("<strike>", L));
tidy("<span style=\"font-style: italic;\">" ++ T, S, L) ->
    tidy(T, ["</i>"|S], reverse("<i>", L));
tidy("</span>" ++ T, [Final|S], L) ->
    tidy(T, S, reverse(Final, L));
tidy([H|T], S, L) ->
    tidy(T, S, [H|L]);
tidy([], S, L) ->
    reverse(L).


