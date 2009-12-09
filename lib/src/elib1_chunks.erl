%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(elib1_chunks).
-compile(export_all).
-import(lists, [filter/2, flatten/1, reverse/1]).
-import(elib1_misc, [dump/2]).

%% convert .log file to .html
%% by converting each chunk in the log file

batch(X) ->
    try
	begin 
	    [A] = X,
	    File = atom_to_list(A),
	    convert(File)
	end
    catch
	P:Q ->
	    io:format("Some error ocurred ~p:~p ~p~n",
		      [P,Q,erlang:get_stacktrace()])
    end.

file2chunks(File) ->
    Str = elib1_misc:file2string(File),
    str2chunks(Str).

convert(File) ->
    Root = filename:rootname(File),
    Chunks = file2chunks(File),
    %% dump("logtmp", Chunks),
    H1 = [ chunk2html(C) || C <- Chunks],
    OutFile = "../doc/" ++ Root ++ ".html",
    elib1_misc:expand_file_template("me.template", 
				    [{"content", H1}], 
				    OutFile),
    io:format("created ~s~n",[OutFile]).

chunk2html({chunk,Headers,Content}) ->
    div_box([headers2html(Headers), content2html(Content)]).

content2html(Str) ->
    wikiA2html(parse_wiki_str(Str)).

headers2html(L) ->
    [h2([atom_to_list(Key),": ",Val]) || {Key,Val} <- L].

h2(X) ->
    ["<h2>",X,"</h2>\n"].

div_box(X) ->
    ["<div class='box'>\n", X, "\n</div>"].


test1() ->
    str2chunks("@chunk\n@tag: abc\n\nabc\n\n+123\n+234\n\ndef").


str2chunks(Str) ->
    Str1 = elib1_misc:dos2unix(Str),
    parse_chunks(Str1).

parse_chunks("@chunk" ++ _ = T) ->
    parse_chunks(T, 1, []);
parse_chunks(Str) ->
    io:format("**** string does not begin with @chunk~n"
	      " starts:~s ...~n"
	      " skipping some data~n",[string:sub_string(Str,1,10)]),
    {_, Ln1, Str1} = collect_chunk(Str, 1, []),
    parse_chunks(Str1, Ln1, []).

parse_chunks([], _, L) ->
    reverse(L);
parse_chunks("@chunk" ++ Str, Ln, L) ->
    {C, Ln1, Str1} = collect_chunk(Str, Ln, []),
    {Header, Body} = parse_chunk_headers(C, Ln, []),
    parse_chunks(Str1, Ln1, [{chunk,Header,Body}|L]).

parse_chunk_headers("\n\n" ++ T, _Ln, L) ->
    {reverse(L), T};
parse_chunk_headers("\n@" ++ T, Ln, L) ->
    %% io:format("isolate tag:~p~n",[T]),
    {Tag, T1} = isolate_meta_tag(T, Ln, []),
    %% io:format("Tag=~p T1=~s~n",[Tag, string:sub_string(T1,1,10)]),
    {Body, Ln1, T2} = collect_tag_body(T1, Ln+1, []),
    parse_chunk_headers(T2, Ln1, [{Tag,trim(Body)}|L]).

trim(X) ->
    elib1_misc:remove_leading_and_trailing_whitespace(X).

-define(IN(A,X,B), A =< X, X =< B).

isolate_meta_tag([H|T], Ln, L) when ?IN($a,H,$z) ; ?IN($A,H,$Z) ->
    isolate_meta_tag(T, Ln, [H|L]);
isolate_meta_tag([$:|T], _, L) ->
    {list_to_atom(reverse(L)), T};
isolate_meta_tag(Str, Ln, L) ->
    exit({ebadTag,Ln,string:sub_string(Str,1,10),reverse(L)}).

collect_tag_body("\n@" ++ _ = T, Ln, L) -> {reverse(L), Ln, T};
collect_tag_body("\n\s" ++ T, Ln, L)    ->
    collect_tag_body(T, Ln+1, L);
collect_tag_body("\n\n" ++ _ = T, Ln, L) ->
    {reverse(L), Ln, T};
collect_tag_body("\n" ++ _T, Ln, L) ->
    exit({ebadTag, Ln, reverse(L)});
collect_tag_body([H|T], Ln, L) ->
    collect_tag_body(T, Ln, [H|L]);
collect_tag_body([], Ln, L) ->
    {reverse(L), Ln, []}.

collect_chunk("\n@chunk" ++ _ = T, Ln, L) ->
    {reverse(L), Ln+1, tl(T)};
collect_chunk([$\n|T], Ln, L) ->
    collect_chunk(T, Ln+1, [$\n|L]);
collect_chunk([H|T], Ln, L) ->
    collect_chunk(T, Ln, [H|L]);
collect_chunk([], Ln, L) ->
    {reverse(L), Ln, []}.

parse_wiki_str(Str) ->
    parse_str0(Str, 0).
    
parse_str0(Str, Ln) ->
    Pass1 = parse(Str, Ln, []),
    %% dump("t1", Pass1),
    Pass2 = flatten([pass2(I) || I <- Pass1]),
    [pass3(I) || I <- Pass2].

pass2({str,_Ln,L})    -> split_into_paras(L);
pass2({dl,_Ln,L})     -> parse_dl(L);
pass2({pre,_Ln,L})    -> {pre,L};
pass2(X)              -> exit({pass2, X}).

pass3({para,[]})         -> drop;
pass3({para,"+" ++ _ =T}) -> parse_list1(ol, "\n\\+", [$\n|T]);
pass3({para,"*" ++ _ =T}) -> parse_list1(ul, "\n\\*", [$\n|T]);
pass3({para,"=" ++ S}) -> parse_header(S, 1);
pass3({para,S})        -> parse_para(S);
pass3(X)               -> X.

parse_list1(Tag, Re, Str) ->
    L = re:split(Str, Re, [{return,list}]),
    L1 = remove_blank_lines(L),
    L2 = [parse_para(I) || I <- L1],
    {Tag, L2}.

parse_dl(S) ->
    %% look for lines that start \n
    %% io:format("parse_dl =~p~n",[S]),
    Lines = split_into_regions(S),
    %% io:format("Lines =~p~n",[Lines]),
    Lines1 = [parse_dl_item(I) || I <- Lines],
    %% io:format("Lines1 =~p~n",[Lines1]),
    {dl, Lines1}.
    
parse_dl_item(Str) ->
    {Tag, Rest} = extract_tag(Str, []),
    {tag,Tag,paras,parse_paras(split_into_paras(Rest))}.

extract_tag([$\n|_],_)     -> exit(nlNotAllowedInTag);
extract_tag([], _)         -> exit(eofInTag);
extract_tag([$\\,$]|T], L) -> extract_tag(T, [$]|L]);
extract_tag([$]|T], L)     -> {reverse(L), T};
extract_tag([H|T], L)      -> extract_tag(T, [H|L]).

split_into_regions(S) ->
    L = re:split(S, "[\r]?\n[\s\t]*\\[",[{return,list}]),
    remove_blank_lines(L).


%% split_into_para(Str) -> [Str].

split_into_paras(S) ->
    L = re:split(S, "[\r]?\n[\s\t]*[\r]?\n",[{return,list},trim]),   
    L1 = remove_blank_lines(L),
    [{para,elib1_misc:remove_leading_whitespace(I)} || I <- L1].

remove_blank_lines(L) ->
    filter(fun(I) -> not elib1_misc:is_blank_line(I) end, L).


%% collect top-level objects (only <dl> and <pre> in column one

parse([], _, L)               -> reverse(L);
parse("\n<dl>" ++ T, Ln, L)   -> parse1(dl, T, "</dl>", Ln+1, L);
parse("\n<pre>" ++ T, Ln, L)  ->
    %% Pre **must** have a WS /n
    T1 = skip_to_pre_start(T, Ln),
    io:format("Here:Ln=~p T1=~p~n",[Ln,T1]),
    parse1(pre, T1, "\n</pre>", Ln+1, L);
parse(Str, Ln, L) ->
    {Body, Ln1, T1} = collect_str(Str, Ln, []),
    parse(T1, Ln1, [{str,Ln,Body}|L]).

skip_to_pre_start([$\n|T], _)  -> T;
skip_to_pre_start([$\s|T], Ln) -> skip_to_pre_start(T, Ln);
skip_to_pre_start([$\t|T], Ln) -> skip_to_pre_start(T, Ln);
skip_to_pre_start([$\r|T], Ln) -> skip_to_pre_start(T, Ln);
skip_to_pre_start(_Str, Ln) ->
    exit({pre,line,Ln,nonBlankOnSameLine}).

collect_str("\n<pre>" ++ _ = T, Ln, L)  -> {reverse(L), Ln+1, T};
collect_str("\n<dl>" ++ _ = T, Ln, L)   -> {reverse(L), Ln+1, T};
collect_str([$\n|T], Ln, L)             -> collect_str(T, Ln+1, [$\n|L]);
collect_str([H|T], Ln, L)               -> collect_str(T, Ln, [H|L]);
collect_str([], Ln, L)                  -> {reverse(L), Ln, []}.

parse1(Tag, Str, Stop, Ln, L) ->
    {Body, Ln1, Str1} = collect_thing(Str, Stop, Ln, []),
    parse(Str1, Ln1, [{Tag,Ln,Body}|L]).

collect_thing([H|T] = Str, Stop, Ln, L) ->
    case elib1_misc:is_prefix(Stop, Str) of
	{yes, Rest} -> 
	    {reverse(L), Ln + count_nls(Stop), Rest};
	no  -> 
	    collect_thing(T, Stop, bump(H, Ln), [H|L])
    end;
collect_thing([], Stop, Ln, _) ->
    exit({eof,line,Ln,expecting,Stop}).

count_nls([$\n|T]) -> 1 + count_nls(T);
count_nls([_|T])   -> count_nls(T);
count_nls([])      -> 0.

bump($\n, N) -> N+1;
bump(_,   N) -> N.

parse_header([$=|T], N) -> parse_header(T, N+1);
parse_header(T, N)      -> {header, N, T}.

parse_paras(L) ->
    [parse_para(I) || {para,I} <- L].

parse_para(Str) ->
    {p, parse_para(Str, [])}.

parse_para([], L)                -> reverse(L);
parse_para("<br/>" ++ T, L)      -> parse_para(T, [br|L]);
parse_para("<u>" ++ T, L)        -> parse_para1(u, T, "</u>", L);
parse_para("<tt>" ++ T, L)       -> parse_para1(u, T, "</tt>", L);
parse_para("<<" ++ T, L)         -> parse_para1(code, T, ">>", L);
parse_para("<code>" ++ T, L)     -> parse_para1(code, T, "</code>", L);
parse_para("<footnote>" ++ T, L) -> parse_para1(footnote, T, "</footnote>", L);
parse_para("''" ++ T, L)         -> parse_para1(i, T, "''", L);
parse_para("**" ++ T, L)         -> parse_para1(b, T, "**", L);
parse_para("[[" ++ T, L)         -> parse_para1(link, T, "]]", L);
parse_para("<s>" ++ T, L)        -> parse_para2(strike, T, "</s>", L);
parse_para("~~" ++ T, L)         -> parse_para2(strike, T, "~~", L);
parse_para("<warn>" ++ T, L)     -> parse_para2(warn, T, "</warn>", L);
parse_para("\"" ++ T, L)         -> parse_para2(quoted,T, "\"", L);
parse_para(T, L) ->
    {B, T1} = collect_str(T, []),
    parse_para(T1, [{str,B}|L]).

collect_str("[[" ++ _ = T, L)         -> {reverse(L),  T};
collect_str("''" ++ _ = T, L)         -> {reverse(L),  T};
collect_str("**" ++ _ = T, L)         -> {reverse(L),  T};
collect_str("<tt>" ++ _ = T, L)       -> {reverse(L),  T};
collect_str("<s>" ++ _ = T, L)        -> {reverse(L),  T};
collect_str("<br/>" ++ _ = T, L)      -> {reverse(L),  T};
collect_str("~~" ++ _ = T, L)         -> {reverse(L),  T};
collect_str("<u>" ++ _ = T, L)        -> {reverse(L),  T};
collect_str("<code>" ++ _ = T, L)     -> {reverse(L),  T};
collect_str("<warn>" ++ _ = T, L)     -> {reverse(L),  T};
collect_str("<footnote>" ++ _ = T, L) -> {reverse(L),  T};
collect_str("<<" ++ _ = T, L)         -> {reverse(L),  T};
collect_str("\"" ++ _ = T, L)         -> {reverse(L),  T};
collect_str([H|T], L)                 -> collect_str(T, [H|L]);
collect_str([], L)                    -> {reverse(L), []}.

parse_para1(Tag, Str, Stop, L) ->
    {B, Str1} = collect(Str, Stop, []),
    parse_para(Str1, [{Tag,B}|L]).

parse_para2(Tag, Str, Stop, L) ->
    {B, Str1} = collect(Str, Stop, []),
    parse_para(Str1, [{Tag,parse_para(B)}|L]).

collect([], _, L) ->
    {reverse(L), []};
collect([$\\,H|T], Stop, L) ->
    collect(T, Stop, [H|L]);
collect(Str, Stop, L) ->
    case elib1_misc:is_prefix(Stop, Str) of
	{yes, Rest} ->
	    {reverse(L), Rest};
	no ->
	    collect(tl(Str), Stop, [hd(Str)|L])
    end.


wikiA2html({header,N,S}) ->
    M = integer_to_list(N+1),
    ["<h",M,">",quote(S),"</h",M,">\n"];
wikiA2html({p, L}) ->
    ["<p>",inlines2html(L),"</p>\n"];
wikiA2html({pre, L}) ->
    ["<pre>\n", quote(L),"\n</pre>\n"];
wikiA2html({dl, L}) ->
    ["<dl>\n", dl_body_to_html(L),"\n</dl>\n"];
wikiA2html({ol, L}) ->
    ["<ol>",[["<li>", inlines2html(I),"</li>\n"] || {p, I} <-L],"</ol>\n"];
wikiA2html({ul, L}) ->
    ["<ul>",[["<li>",inlines2html(I),"</li>\n"]|| {p, I} <- L],"</ul>\n"];
wikiA2html(L) when is_list(L) ->
    [wikiA2html(I) || I <- L];
wikiA2html(X) ->
    io:format("wikiA2html???:~p~n",[X]),
    pre(X).

list_paras([{p,P}|T]) ->
    %% the first para has no <p> .. </p> wrapper the rest do
    [inlines2html(P) | [wikiA2html(I) || I <- T]].

dl_body_to_html(L) ->
    [["<dt><b>",Tag,"</b></dt>"|dl_body1(Ps)] || {tag,Tag,paras,Ps} <- L].

dl_body1([{p,H}|T]) ->
    ["<dd>",inlines2html(H),"\n" , [wikiA2html(I) || I <- T],"</dd>\n"].

li(X) ->
    ["<li>",quote(X),"</li>\n"].

pre(X) ->
    L = lists:flatten(io_lib:format("**~p~n", [X])),
    ["<pre>\n",quote(L),"\n</pre>\n"].

inlines2html(L) -> [inline2html(I)||I<-L].

inline2html(br)             -> "</br>\n";
inline2html({link,L})       -> ["<a href='",L,"'>",L,"</a>"];
inline2html({strike,{p,L}}) -> ["<strike>",inlines2html(L),"</strike>"];
inline2html({warn,{p,L}})   -> ["<font color='red'>",inlines2html(L),"</font>"];
inline2html({quoted,{p,L}}) -> ["&ldquo;",inlines2html(L),"&rdquo;"];
inline2html({footnote,L})   -> ["<span class='footnote'>",quote(L),"</span>"];
inline2html({b, I})         -> ["<b>",quote(I),"</b>"];
inline2html({i, I})         -> ["<i>",quote(I),"</i>"];
inline2html({bi, I})        -> ["<b><i>",quote(I),"</i></b>"];
inline2html({u, I})         -> ["<underline>",quote(I),"</underline>"];
inline2html({code, I})      -> ["<tt>",quote(I),"</tt>"];
inline2html({tt, I})        -> ["<tt>",quote(I),"</tt>"];
inline2html({str, I})       -> quote(I);
inline2html(X)              -> pre({unexpected,inline,X}).

quote("<" ++ T)   -> "&lt;" ++ quote(T);
quote("&" ++ T)   -> "&amp;" ++ quote(T);
quote("'" ++ T)   -> "&rsquo;" ++ quote(T);
quote("---" ++ T) -> "&mdash;" ++ quote(T);
quote("--" ++ T)  -> "&ndash;" ++ quote(T);
quote([H|T])      -> [H|quote(T)];
quote([])         -> [].


