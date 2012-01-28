%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(my_simple_server).

-import(elib1_webkit, [pre/1, mime/1, forever/0,get_file/1,classify/1]).
-import(lists, [reverse/1,sort/1]).

-include_lib("kernel/include/file.hrl").

-compile(export_all).

start([AP,AD]) ->
    Port = list_to_integer(atom_to_list(AP)),
    Dir = filename:dirname(filename:dirname(atom_to_list(AD))),
    io:format("Starting server on port:~w Root=~p~n",[Port,Dir]),
    Env = [],
    elib1_webkit:
	start_fold_server(2068,
			  fun(Tag, Uri, Args, State) ->
				  io:format("handle:~p ~p ~p~n",[Tag,Uri,Args]),
				  handle(Tag, Uri, Args, Dir, State)
			  end,
			  Env),
    forever().

is_prefix([], _) -> true;
is_prefix([H|T1], [H|T2]) -> is_prefix(T1, T2);
is_prefix(_, _) -> false.

version_list(Id, Dir) ->
    {ok, L} = file:list_dir(Dir),
    L1 = order([filename:rootname(I) || I <- L, is_prefix(Id, I)]),
    %% [H|L2] = reverse(L1),
    %% [load_content(H)| [load_content(I) || I <- L2]].
    [["<option>",I,"</option>"] || I <- reverse(L1)].

load_content(I) ->
    ["<a href='#' onclick=\"load_content('",I,"');\">",
     I,"</a><br>"].


order(L) ->
    L1 = sort([{guid_to_list(I),I}||I <- L]),
    [J || {_,J} <- L1].

next_rev(X) ->
    [Doc,RevStr] = string:tokens(X,"v"),
    Rev = list_to_integer(RevStr),
    Doc  ++ "v" ++ integer_to_list(Rev + 1).

document(X) ->
    [Doc|_] = string:tokens(X, "d"),
    Doc.

guid_to_list(L) ->
    [list_to_integer(I) || I <- string:tokens(L, "_vd")].

handle(Op, "/eval", [{"mod",Mod}|Args], D, E) ->
    %% Just deligate
    Mod:handle(Op, Args, D, E);

handle(Op, "/getFile", [{"arg",File}], D, E) ->
    case elib1_webkit:serve_static_file_report_error(File) of
	{response,Type,Val} ->
	    {response,Type,Val,E};
	{error, C} ->
	    {error, C, E}
    end;

handle(_, "/listRoot", Dir, _, E) ->
    %% fusk
    HTML = "<ol>
  <li class='ui-finder-folder'><a href='DirName.html'>
  Main</a>
  </li>
  <p>Hi Joe Here we are</p>

  <li class='ui-finder-file'>
    <a href='html/somefile.html?0.jpg' title='some title for this image'>
    Lorem ipsum dolor sit amet </a>
  </li>
  <li class='ui-finder-file'>
    <a href='html/somefile.html?1'>Consectetur adipiscing elit</a>
  </li>
  <li>
    <a>Nam auctor, lectus vulputate</a>
    <ul>
      <li><a>Some fileasd asd asdasd a</a></li>
      <li><a href='html/somefile.html?4324432428.jpg'>Some Imaged asd as</a></li>
      <li><a>Some Folder</a>
      <ul>
	<li><a href='html/somefile.html?7321343'>Some fileasd asd asdasd a</a></li>
	<li><a href='html/somefile.html?433123248.jpg'>Some Imaged asd as</a></li>
	<li><a href='html/level4.html?fsdfs'>Some file</a></li>
      </ul>
      </li>
    </ul>
  </li>
  <li class='ui-finder-file'><a href='html/somefile.html?2'>Consequat luctus</a></li>
  <li class='ui-finder-file'><a href='html/somefile.html?3'>Nisl enim eleifend sem</a></li>
  <li class='ui-finder-folder'><a href='html/level2.html'>Nam auctor, lectus vulputate</a></li></ol>	",
     {response, html, HTML, E};


handle(_, "/get_versions", [{"id",Id}], Dir, E) ->
    %% return a list of all filenames with this prefix
    H = version_list(Id, Dir),
    {response, html, H, E};
handle(_,"/initialize", [], Dir, E) ->
    %% Need to know several things
    SubDirs = "<option>..</option><option>abc</option>",
    Ret = rfc4627:encode({obj,[{"cwd",list_to_binary(Dir)},
			       {"subdirs", list_to_binary(SubDirs)},
			       {"doc",<<"foo123">>},
			       {"versions",[<<"a">>,<<"b">>]}]}),
    {response, json, Ret, E};

handle(_, "/load_content", [{"id",Id}], Dir, E) ->
    {ok, Bin} = file:read_file(Full = Id ++ ".html"),
    M = io_lib:format("~p", [filelib:last_modified(Full)]),
    %% check if the next version is free
    Id1 = next_rev(Id),
    io:format("Id=~p next=~p~n",[Id,Id1]),
    Edit = not filelib:is_file(Id1 ++ ".html"),
    Ret = rfc4627:encode({obj,[{"guid",list_to_binary(Id)},
			       {"edit", Edit},
			       {"mod", list_to_binary(M)},
			       {"val",Bin}]}),
    {response, json, Ret, E};

handle(_, "/save_content", [{"id",Id},{"value",Val}], Dir, E) ->
    Id1 = next_rev(Id),
    Full = Id1 ++ ".html",
    Ret = case filelib:is_file(Full) of
	      false ->
		  io:format("Saving to:~p~n",[Full]),
		  file:write_file(Full, [Val]),
		  M = io_lib:format("~p",[filelib:last_modified(Full)]),

		  Stem = document(Id1),
		  Vsns = version_list(Stem, Dir),
		  rfc4627:encode({obj,[{"guid",list_to_binary(Id1)},
				       {"versions", list_to_binary(Vsns)},
				       {"edit", true},
				       {"mod", list_to_binary(M)},
				       {"val",list_to_binary(Val)}]});
	      true ->
		  %% nned to create a cloan ..
		  rfc4627:encode({obj,[{"guid",<<"error">>},
				       {"val",<<"cloan">>}]})
	  end,
    {response, json, Ret, E};
handle(Op ,"/file&src=" ++ F, Args, Dir, E) ->
   case elib1_webkit:serve_static_file_report_error(F) of
	{response,Type,Val} ->
	   {response,Type,Val,E};
	{error, C} ->
	   {error, C, E}
    end;
handle(_, "/get_images", [], Dir, E) ->
    Dirs = find_dirs(Dir),
    Files = elib1_find:files(Dir, "*.jpg", false),
    io:format("Files=~p~n",[Files]),
    Ret = [["<img width='100' src='/file&src=",I,"'>"]|| I <- Files],
    {response, html, Ret, E};
handle(Op, File0, Args, Dir, E) ->
    File = Dir ++ File0,
%%     io:format("Op=~p ((Dir=~p File0=~p)) File=~p Args=~p~n",
%% 	      [Op, Dir, File0, File, Args]),
    case file:read_file_info(File) of
	{ok, Info} ->
	    case Info#file_info.type of
		regular ->
		    case file:read_file(File) of
			{ok, Bin}  ->
			    %% io:format("File ~p is ~p~n",[File,classify(File)]),
			    {response, classify(File), [Bin], E};
			{error, _} ->
			    io:format("** missing file:~p~n",[File]),
			    {error, 400, E}
		    end;
		directory ->
		    io:format("calling file_lister~n"),
		    Html = file_lister:list1(Dir, File0),
		    %% io:format("Result=~p~n",[Html]),
		    {response, html, Html, E};
		_ ->
		    {error, 400, E}
	    end;
	{error, _} ->
	    {error, 400, E}
    end.

find_dirs(D) ->
    [".."].

