%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(elib1_docmaker).
%% -compile(export_all).
-import(lists, [filter/2,flatten/1, reverse/1, reverse/2]).
-import(elib1_misc, [string2latex/1]).

-export([test/0, batch/1, convert/1, 
	 color_erlang_code/1,
	 str2wikiA/1,
	 wikiA2html/1, wikiA2latex/1]).

test() ->
    convert("./design.chap"),
    convert("./lorum.chap"),
    convert("./book.book").

batch(X) ->
    io:format("Batch:~p~n",[X]),
    try
	begin 
	    [A] = X,
	    File = atom_to_list(A),
	    convert(File)
	end
    catch
	P:Q ->
	    io:format("Some error ocurred ~p:~p~n",[P,Q])
    end.

convert(File) ->
    case filename:extension(File) of
	".chap" -> convert(File, chapter);
	".book" -> convert(File, book)
    end.
	    
%% START:tag1
convert(File, Type) ->
    Root = filename:rootname(File),
    Str = elib1_misc:file2string(File),
    Pass3 = str2wikiA(Str),
    Debug = "../tmp/" ++ Root,
    elib1_misc:dump(Debug, Pass3),
    Html = wikiA2html(Pass3),
    HtmlFile = "../doc/" ++ Root ++ ".html",
    file:write_file(HtmlFile, [Html]),
    io:format("created ~s~n",[HtmlFile]),
    Latex = wikiA2latex(Pass3),
    LatexFile = "../tmp/" ++ Root ++ "_inc.tex",
    %% elib1_misc:dump("temp", Latex),
    %% elib1_misc:check_io_list(Latex),
    Ret1 = file:write_file(LatexFile, [Latex]),
    io:format("created ~s Ret1=~p~n",[LatexFile,Ret1]),
    TexFile = "../tmp/" ++ Root ++ ".tex",
    io:format("created ~s~n",[TexFile]),
    make_tex(TexFile, Root ++ "_inc", Type),
    make_pdf(Root).

str2wikiA(Str) -> {wikiA, parse_str0(Str)}.

parse_str0(Str) ->
    Pass1 = parse(Str, 1, []),
    Pass2 = flatten([pass2(I) || I <- Pass1]),
    [pass3(I) || I <- Pass2].

make_pdf(File) ->
    os:cmd("cd ../tmp; pdflatex; pdflatex " ++ File),
    file:rename("../tmp/" ++ File ++ ".pdf",
		"../doc/" ++ File ++ ".pdf").

%% END:tag1
%%     Pass2 = [pass2(I) || I <- Pass1],
%%     elib1_misc:dump("pass2", Pass2).

pass2({str,_Ln,L})    -> split_into_paras(L);
pass2({dl,_Ln,L})     -> parse_dl(L);
pass2({ol,_Ln,L})     -> {ol, parse_list(L)};
pass2({ul,_Ln,L})     -> {ul, parse_list(L)};
pass2({note,_Ln,Str}) -> {note,parse_str0(Str)};
pass2({include,Ln,L}) -> parse_include(L, Ln);
pass2({pre,_Ln,L})    -> {pre,L};
pass2(X)              -> exit({pass2, X}).

pass3({para,[]})       -> drop;
pass3({para,"=" ++ S}) -> parse_header(S, 1);
pass3({para,S})        -> parse_para(S);
pass3(X)               -> X.

parse_header([$=|T], N) -> parse_header(T, N+1);
parse_header(T, N)      -> {header, N, T}.

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

 
%% we only have the following top level tags
%% <ul> ... </ul> <dl> .. </dl>  <pre>...</pre> <include .... />
%% NOTE: If you add a new top level tag remember to fix collect_str/3 as
%% well

parse([], _, L)               -> reverse(L);
parse("<note>" ++ T, Ln, L)   -> parse1(note, T, "</note>", Ln, L);
parse("<ol>" ++ T, Ln, L)     -> parse1(ol, T, "</ol>", Ln, L);
parse("<ul>" ++ T, Ln, L)     -> parse1(ul, T, "</ul>", Ln, L);
parse("<dl>" ++ T, Ln, L)     -> parse1(dl, T, "</dl>", Ln, L);
parse("<pre>" ++ T, Ln, L)    -> parse1(pre, T, "</pre>", Ln, L);
parse("<include" ++ T, Ln, L) -> parse1(include, T, "/>", Ln, L);
parse(Str, Ln, L) ->
    {Body, Ln1, T1} = collect_str(Str, Ln, []),
    parse(T1, Ln1, [{str,Ln,Body}|L]).

collect_str("<include" ++ _ = T, Ln, L) -> {reverse(L), Ln, T};
collect_str("<pre>" ++ _ = T, Ln, L)    -> {reverse(L), Ln, T};
collect_str("<dl>" ++ _ = T, Ln, L)     -> {reverse(L), Ln, T};
collect_str("<ol>" ++ _ = T, Ln, L)     -> {reverse(L), Ln, T};
collect_str("<ul>" ++ _ = T, Ln, L)     -> {reverse(L), Ln, T};
collect_str("<note>" ++ _ = T, Ln, L)   -> {reverse(L), Ln, T};
collect_str([$\n|T], Ln, L)             -> collect_str(T, Ln+1, [$\n|L]);
collect_str([H|T], Ln, L)               -> collect_str(T, Ln, [H|L]);
collect_str([], Ln, L)                  -> {reverse(L), Ln, []}.

parse1(Tag, Str, Stop, Ln, L) ->
    {Body, Ln1, Str1} = collect_thing(Str, Stop, Ln, []),
    parse(Str1, Ln1, [{Tag,Ln,Body}|L]).

collect_thing([$\n|T], Stop, Ln, L) ->
    collect_thing(T, Stop, Ln+1, [$\n|L]);
collect_thing([H|T] = Str, Stop, Ln, L) ->
    case elib1_misc:is_prefix(Stop, Str) of
	{yes, Rest} -> {reverse(L), Ln, Rest};
	no          -> collect_thing(T, Stop, Ln, [H|L])
    end;
collect_thing([], Stop, Ln, _) ->
    exit({eof,line,Ln,expecting,Stop}).

%% split_into_para(Str) -> [Str].

split_into_paras(S) ->
    L = re:split(S, "[\r]?\n[\s\t]*[\r]?\n",[{return,list},trim]),   
    L1 = remove_blank_lines(L),
    [{para,elib1_misc:remove_leading_whitespace(I)} || I <- L1].

remove_blank_lines(L) ->
    filter(fun(I) -> not elib1_misc:is_blank_line(I) end, L).

%% list elements start with a +
split_into_list_elements(S) ->
    L = re:split(S, "[\r]?\n[\s\t]*\\+",[{return,list},trim]),
    remove_blank_lines(L).

parse_list(S) ->
    Elements = split_into_list_elements(S),
    [{li,parse_paras(split_into_paras(I))} || I <- Elements].

parse_paras(L) ->
    [ parse_para(I) || {para,I} <- L].

parse_include(S, _Ln) ->    
    try
	begin
	    {ok, Toks, _} = erl_scan:string(S),
	    case lists:sort(parse_include1(Toks)) of
		[{file,File},{tag,Tag}] -> 
		    Str = elib1_misc:get_erl_section(File, Tag),
		    Type = filename:extension(File),
		    {include, File, Type, Str};
		[{chapter,Chap}] ->
		    {includeChapter, Chap}
	    end
	end
    catch
	XX:YY ->
	    exit({ebadInclude, S, XX, YY})
    end.

parse_include1([{atom,_,A},{'=',_},{string,_,S}|T]) ->
    [{A,S}|parse_include1(T)];
parse_include1([]) ->
    [].

parse_para(Str) ->
    {p, parse_para(Str, [])}.

parse_para1(Tag, Str, Stop, L) ->
    {B, Str1} = collect(Str, Stop, []),
    parse_para(Str1, [{Tag,B}|L]).

parse_para2(Tag, Str, Stop, L) ->
    {B, Str1} = collect(Str, Stop, []),
    parse_para(Str1, [{Tag,parse_para(B)}|L]).

parse_para([], L)                -> reverse(L);
parse_para("<br/>" ++ T, L)      -> parse_para(T, [br|L]);
parse_para("<u>" ++ T, L)        -> parse_para1(u, T, "</u>", L);
parse_para("<tt>" ++ T, L)       -> parse_para1(u, T, "</tt>", L);
parse_para("<<" ++ T, L)         -> parse_para1(code, T, ">>", L);
parse_para("<code>" ++ T, L)     -> parse_para1(code, T, "</code>", L);
parse_para("<footnote>" ++ T, L) -> parse_para1(footnote, T, "</footnote>", L);
parse_para("''" ++ T, L)         -> parse_para1(i, T, "''", L);
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

%% mk to lib

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

%%----------------------------------------------------------------------

html_header() ->
    ["<style>"
     "body {margin-left:1in; margin-right:1in; text-align:justify}"
     "div.note { margin-left:1cm; margin-right:1cm; "
     "           padding:10px; background-color:#aaaaaa}"
     "</style>\n"].


wikiA2html({wikiA, L}) ->
    [html_header(),[wikiA2html(I) || I <- L]];
wikiA2html({include,File,_Tag,Str}) ->
    ["<b>",File,"</b>\n<ul><pre><b>",quote(Str),"</b></pre></ul>\n"];
wikiA2html({header,N,S}) ->
    M = integer_to_list(N),
    ["<h",M,">",quote(S),"</h",M,">\n"];
wikiA2html({note,L}) ->
    ["<div class='note'>\n", wikiA2html(L),"</div>\n"];
wikiA2html({p, L}) ->
    ["<p>",inlines2html(L),"</p>\n"];
wikiA2html({pre, L}) ->
    ["<ul><pre><b>\n", quote(L),"\n</b></pre></ul>\n"];
wikiA2html({dl, L}) ->
    ["<dl>\n", dl_body_to_html(L),"\n</dl>\n"];
wikiA2html({nowiki, L}) ->
    L;
wikiA2html({ol, L}) ->
    ["<ol>",[["<li>",list_paras(I),"</li>\n"] ||{li, I} <-L],"</ol>\n"];
wikiA2html({ul, L}) ->
    ["<ul>",[["<li>",list_paras(I),"</li>\n"]|| {li, I} <- L],"</ul>\n"];
wikiA2html({bullet, L}) ->
    ["<ul>\n",[li(I)||I<-L],"\n</ul>\n"];
wikiA2html(drop) -> [];
wikiA2html({includeChapter,X}) ->
    ["<li><a href='",X,".html'>",X,"</a>"];
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

%%----------------------------------------------------------------------

begin_end(Tag, Content) -> 
    ["\\begin{",Tag,"}\n", Content, "\\end{",Tag,"}\n\n"].

wikiA2latex({wikiA, L})          -> wikiA2latex(L);
wikiA2latex(L) when is_list(L)   -> [wikiA2latex(I) || I <- L];
wikiA2latex({p, L})              -> [wikiA2latex(L),"\n\n"];
wikiA2latex({str,S})             -> string2latex(S);
wikiA2latex(drop)                -> [];
wikiA2latex({li,X})              -> ["\\item ", wikiA2latex(X)];
wikiA2latex({i,I})               -> ["{\\sl ", string2latex(I),"}"];
wikiA2latex({footnote,I})        -> ["\\footnote{ ", string2latex(I),"}"];
wikiA2latex({header,N,I})        -> ["\\eee",$a+N-1,"{", string2latex(I),"}"];
wikiA2latex({code,I})            -> ["\\texttt{", string2latex(I),"}"];
wikiA2latex({pre,L})             -> begin_end("verbatim", L);
wikiA2latex({quoted,{p,L}})      -> ["``", [wikiA2latex(I) || I <- L], "''"];
wikiA2latex({dl,L})              -> [wikiA_tag_to_latex(I) || I <- L];
wikiA2latex({ul, L})             -> begin_end("itemize",
				 	[wikiA2latex(I)||I <-L]);
wikiA2latex({ol,L})              -> begin_end("enumerate", [wikiA2latex(I) || I <- L]);
wikiA2latex({strike,{p,L}})      -> ["\\sout{", [wikiA2latex(I) || I <- L], "}"];
wikiA2latex({warn,{p,L}})        -> ["\\textcolor{red}{",[wikiA2latex(I) || I <- L],"}"];
wikiA2latex({includeChapter, X}) -> ["\\input{",X,"_inc}\n"];
wikiA2latex({note, X})           -> begin_end("note", wikiA2latex(X));
wikiA2latex({include,File,".erl",Str}) ->
    ["\\verb+",File,"+\n",
     "\\begin{verbatim}\n",
     Str,
     "\\end{verbatim}\n"];
wikiA2latex(X) ->
    io:format("wikiA2latex:~p~n",[X]),
    ["\\begin{verbatim}\n",
     string2latex(flatten(io_lib:format("~p~n",[X]))),
     "\\end{verbatim}\n"].

wikiA_tag_to_latex({tag,Tag,paras,[P1|Paras]}) ->
    ["\\hangindent=3pc \\hangafter=1\n",
     "\\verb+",Tag,"+\\\\\n",
     wikiA2latex(P1),
     [["\\leftskip=3pc\n",wikiA2latex(I)] || I <- Paras],
     "\n\n"].

%% START:color
color_erlang_code("<" ++ T) -> "&lt;" ++ color_erlang_code(T);
color_erlang_code("&" ++ T) -> "&amp;" ++ color_erlang_code(T);
color_erlang_code("case" ++ T) -> "<bf>case</bf>" ++ color_erlang_code(T);
color_erlang_code([H|T]) -> [H|color_erlang_code(T)];
color_erlang_code([]) -> [].
%% END:color


make_tex(TeXFile, Include, Type) ->
    io:format("make_tex:~p ~p ~p~n",[TeXFile, Include, Type]),
    L  = case Type of
	     book ->
		 template("book",
			  "\\newcommand{\\eeea}[1]{\\chapter{#1}}\n"
			  "\\newcommand{\\eeeb}[1]{\\section{#1}}"
			  "\\newcommand{\\eeec}[1]{\\subsection{#1}}",
			  Include);
	     chapter ->
		 template("article",
			  "\\newcommand{\\eeea}[1]{\\section{#1}}\n"
			  "\\newcommand{\\eeeb}[1]{\\subsection{#1}}"
			  "\\newcommand{\\eeec}[1]{\\subsubsection{#1}}",
			  Include)
	 end,
    ok = file:write_file(TeXFile, L).

%% generic template
%%  parameterised by type

%% remember the double quotes :-)
%%   Oh for ~q<<"...">>

template(Type, Commands, Include) -> [<<"
\\documentclass[a4paper]{">>, Type, <<"}
%% \\usepackage{palatino, lettrine}

\\usepackage{color}
\\usepackage{xcolor,fancybox}
\\usepackage[normalem]{ulem}

% put fancvyvrb last otherwise it doesn't work
\\usepackage{fancyvrb}

\\newcommand{\\code}[1]{\\texttt{#1}}

">>, Commands, <<"

\\newcommand{\\strike}[1]{\\setbox1\\hbox{#1}\\rule[.5ex]{\\wd1}{.4pt}\\hspace*{-\\wd1}{\\box1}}

\\definecolor{gray75}{gray}{0.75}

\\newenvironment{note}{
  \\begin{Sbox}
    \\begin{minipage}{.9\\columnwidth}}{
    \\end{minipage}
  \\end{Sbox}
  \\bigskip
  \\noindent
  \\setlength{\\fboxsep}{.05\\columnwidth}
  \\colorbox{gray75}{\\TheSbox}
  \\bigskip}
\\begin{document}
\\input{">>, Include, <<"}
\\end{document}">>].


