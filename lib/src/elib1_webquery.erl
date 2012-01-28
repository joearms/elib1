%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(elib1_webquery).
-compile(export_all).

qu([{"name",Collection},{"query",Str}], Root) ->
    Name = Collection,
    Dir = Root,
    {Hits,Pages} = elib1_indexer:q(Dir,Name,Str),
    L1 = [[Str,"&nbsp;",integer_to_list(N)," hits<br>"] || {Str,N} <- Hits],
    L2 = [["<li><a href='/mod?mod=elib1_webquery&func=show_file&name=",
	   Collection,"&pos=",
	   integer_to_list(Pos),"'>",Loc,"</a></li>"] || {Pos,Loc} <- Pages],
    {response, html,
     ["<p>Root:",Root,"<p>Query collection:",Collection," str:",Str,
      "<p>",L1,"<p>",L2]}.

show_file([{"name",Collection},{"pos",Pos}], Root) ->
    Dir = Root,
    Name = Collection,
    Index = list_to_integer(Pos),
    {File, Val} = elib1_indexer:extract(Dir, Name, Index),
    {response, html, ["<pre><b>",Val,"<b></pre>"]}.

