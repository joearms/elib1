%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(file_lister).

-compile(export_all).
-import(lists, [flatten/1, map/2, reverse/1, sort/1]).
-include_lib("kernel/include/file.hrl").

remove_trailing_slash(L) ->
    case reverse(L) of
	"/" ++ L1 -> reverse(L1);
	_ -> L
    end.

list1(BaseDir, RelDir) ->
    Files = list_dir(BaseDir, RelDir),
    Parent = filename:dirname(RelDir),
    %% Back = ["<button onclick='listDir(\"",Parent,"\")'>back</button><p>"],
    L1 = [format_dir(RelDir, I)  || {dir, I} <- Files],
    L2 = [format_text(RelDir, I) || {text,I} <- Files],
    L3 = [format_img(BaseDir, RelDir, I)  || {img,I} <- Files],
    L4 = [format_pdf(RelDir, I)  || {pdf,I} <- Files],
    ["dir=",RelDir," ",
     %% Back,
     "<p>",L1,"<p>",L2,"<p>",L3,"<p>",L4].

mkfull("/", I) -> "/" ++ I;
mkfull(Dir, I) -> Dir ++ "/" ++ I.
    

format_dir(Dir, I) ->
    Full = mkfull(Dir,  I),
    [" <table style='display:inline;'>"
     "<tr><td><img src='/include/images/dir_icon.jpg'></td></tr>",
     "<tr><td><a href='",Full,"'>",I,"</a></td></tr></table>"].

format_pdf(Dir, I) ->
    Full = Dir ++ "/" ++ I,
    ["<table style='display:inline;'>"
     "<tr><td><img src='/include/images/pdf_icon.jpg'></td></tr>",
     "<tr><td><a href='",Full,"'>",I,"</a></td></tr></table>"].

format_text(Dir, I) ->
    Full = mkfull(Dir, I),
    ["<a href='",Full,"'>",I,"</a>\n"].

format_img(BaseDir, Dir, I) ->
    Full = mkfull(Dir,I),
    %% Full = /a/b/c/d.jpg
    Abs = BaseDir ++ Full,
    AbsThumb = filename:rootname(Abs) ++ "_thumb.jpg",
    case filelib:is_file(AbsThumb) of
	false ->
	    Cmd = flatten(["convert  -resize '60x60!' \"",
			   Abs,"\" \"", AbsThumb, "\""]),
	    io:format("Caching:~s~n",[I]),
	    _Ret = os:cmd(Cmd);
	    %% ; io:format("Ret =~p~n",[_Ret]);
	true ->
	    void
    end,
    Thumb = filename:rootname(Full) ++ "_thumb.jpg",
    ["<a href='",Full,"'><img border='0' src='", Thumb, "'></a>"].
     

list_dir(BaseDir, SubDir) ->
    %% lists files in BaseDir ++ SubDir
    %% eg. if     BaseDir = /Users/joe/foo
    %%     and    SubDir = /bar
    %% Then return a list of files (I) relative to SubDir
    %% The abs names of these files is BaseDir ++ SubDir ++ "/"  ++ I
    D1 = BaseDir ++ SubDir,
    case file:list_dir(D1) of
	{ok, Objs} ->
	    %% these are relative to D1
 	    map(fun(File) ->
			Abs = D1 ++ "/" ++ File,
			{classify(Abs, File), File}
		end, Objs);
	{error, _} ->
	    []
    end.

classify(Abs, File) ->
    case file:read_file_info(Abs) of
	{ok, Info} ->
	    case Info#file_info.type of
		regular ->
		    classify_file(File);
		directory ->
		    case File of
			"." ++ _ -> junk;
			_        -> dir
		    end;
		_ ->
		    junk
            end;
        {error, _} ->
	    junk
    end.

classify_file("#" ++ _) -> junk;
classify_file(File) ->
    %% io:format("File=~p~n",[File]),
    case reverse(File) of
	"gpj.bmuht_" ++ _ -> junk;
	"~" ++ _ -> junk;
	_ ->
	    Ext = elib1_misc:to_lower(filename:extension(File)),
	    case Ext of
		[] -> text;
		".pdf" -> pdf;
		".gif" -> img;
		".jpg" -> img;
		".jpeg"-> img;
		".png" -> img;
		".js"  -> text;
		".html" -> text;
		".erl" -> text;
		".sh" -> text;
		".c" -> text;
		".txt" -> text;
		_ -> junk
	    end
    end.

