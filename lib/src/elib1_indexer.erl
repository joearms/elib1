%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

%% @author Joe Armstrong <erlang@gmail.com>
%% @copyright 2009 Joe Armstrong
%% @doc A full-text indexing engine. The functions in this library are
%% modeled after the algorithms in the book "Managing Gigabytes" by
%% I.A.Witten, A.Moffat and T.C. Bell, 2'nd edition.
%% Morgan Kaufmann Publishing, 1999.
%% <h2>Unix Commands</h2>
%% ==eindex==
%% There are a number of top level UNIX commands to perform indexing.
%% <pre>
%% $ eindex -crawl Name
%%     Input Name.in     Output: Name.files and Name.crawl
%% $ eindex -index Name
%%     Input: Name.crawl Output: Name.index
%% $ eindex -dump Name
%%     Input Name     Output: Name.tmp
%%     example eindex -dump test2.crawl produces test2.crawl.tmp
%% $ eindex -dumpIndex Name
%%     Input: Name.index Output: Name.index.tmp
%% $ eindex -statstics Name
%%     Input: Name.index Output: Statistics
%% $ eindex -search Name "String"
%%     Input Name.in     
%%     Output: matching objects
%%     example: eindex -search test2 "property lists"
%% $ eindex -help
%%     prints usage
%% </pre>
%% <b>-crawl</b>Is used to gather a collection of files 
%% together prior to indexing.
%% The file <b>Name.in</b> contains a list of directories and file extensions
%% which control the gathering phase. All matching files specified in the
%% input file are read, compressed and appended into a single file
%% called a <b>crawl</b>. This file will be rather large, depending upon the
%% size of the scan - but it contains all the data we need for the subsequent
%% indexing phases. In a typical gather phase, I collect 46,000 Erlang
%% files on my disk and compress them into a single 120 MByte file.
%%
%% <h2>File Formats</h2>
%% ==Crawl files==
%% A file with the extension <b>.crawl</b> is a crawl file.
%% A crawl file is a set of BFTs containing
%% <pre>
%% {Term:any(), Extension::string(), Md5Content::binry(), compressedContent::binary()}
%% </pre>
%% 
%% The indexer uses a number or different file formats.
%% These are contained in files with the extensions <b>.in</b>
%% <b>.crawl</b> and <b>.index</b>.
%% ==Input Data==
%% Files with the extension <b>.in</b> contain a list of
%% tuples with a start directory and a file extension. For example the
%% file <b>test1.in</b> in the examples/indexer directory contains
%% the following:
%% <pre>
%% {"/Users/joe/code/elib2-1", ".erl"}.
%% {"/Users/joe/msi/2005/erl/projects/supported", ".erl"}.
%% </pre>
%%
%% When the first phase of indexing occurs we crawl through the file
%% system looking for all files under the root <b>/Users/joe/code/elib2.1</b>
%% with the file extension <b>.erl</b>. Symbolic links are followed if
%% they point within the root directory, but circular links are not followed.
%% All the Erlang code found is compressed and appended to a so called
%% "crawl" file. The crawl file is a sequence of tuples of the form
%% <pre>
%% {FileName::string(), Md5::binary(), CompressedContent::binary()}  
%% </pre>
%% ==Binary Tuple Format==
%% Binary tuple formats (BTF) occurs a lot.
%% If T is tuple, then the
%% following binary written to disk is in BTF format:
%% <pre>&lt;&lt;(size(tuple_to_binary(T)):32-unsigned-big-integer),
%%         tuple_to_binary(T)>>
%% </pre>
%% Stored on disk in binary tuple format (ie as a 4 byte length header,

-module(elib1_indexer).

-export([make_index/2,
	 crawl_files/1,
	 dump_index/1,
	 do/1,
	 extract/2,
	 list_files/2,
	 lookup/2,
	 q/2, q/3]).

-import(elib1_misc, [time_fun/2]).

%% -compile(export_all).

-import(lists, [foldl/3,reverse/1]).

-type void() :: any(). %% my way of saying I don't care about the result

-spec do(Command::string()) -> no_return().
    
do(Cmd) -> do1(Cmd), init:stop().

do1(["-crawl", Name])       -> crawl_files(Name);
do1(["-dump", Name])        -> dump(Name);
do1(["-dumpIndex", Name])   -> dump_index(Name);
do1(["-statistics", Name])  -> dump_statistics(Name);
do1(["-index", Name])       -> make_index(Name);
do1(["-search", Name, Str]) -> q(Name, Str);
do1(["-help"])              -> usage();
do1(Args) ->
    io:format("eindex unexpected argument:~p~n",[Args]),
    usage().

usage() ->
    io:format("eindex -crawl Name\n"
	      "   build Name.crawl from Name.in\n"
	      "eindex -index Name\n"
	      "   build Name.index from Name.crawl\n"
	      "eindex -debug Name\n"
	      "   output a symbolic version of name\n"
	      "   example eindex -debuf test2.crawl\n"
	      "eindex -debug Name\n"
	      "   output a symbolic version of name\n"
	      "   example eindex -debuf test2.crawl\n"
	      "eindex -dumpIndex Name\n"
	      "   output a symbolic version of Name.index\n"
	      "eindex -statistics Name\n"
	      "   output some statistics about Name.index\n"
	      "eindex -search Name Str\n"
	      "   Search Name.index for Str\n").

-spec crawl_files(Name::string()) -> void().

%% @doc Reads Name.in and finds all the files in the input
%% packing them in a file called Name.crawl. This calls
%% init:stop() when it has finished. 

crawl_files(Name) ->
    InFile = Name ++ ".in",
    case file:consult(InFile) of
	{ok, L} ->
	    io:format("crawling files:~p~n",[L]),
	    list_files(Name, L),
	    pack_files(Name);
	_ ->
	    io:format("cannot consult:~s~n",[InFile])
    end.

%% {nfiles,17539,nbytes,295805314}
%%  packer:pass0_big().
%%    found 42826 files on my disk
%%          17539 are unique which take 295 MB
%%          so the average file size is 16.8 KB
%%          The archive is 59022207 bytes (compressed)
%%          Which means we have a compression factor of
%%          5.
%%

 
make_index(Name, Top) ->
    time_fun("total time to buid index",
	     fun() ->
		     list_files(Name, Top),
		     pack_files(Name),
		     extract_keywords(Name),
		     pack_index(Name),
		     q(Name,"dict sort")
	    end).

-spec make_index(Name::string()) -> void().

%% @doc Reads Name.crawl ...

make_index(Name) ->
    time_fun("indexing:",
	     fun() ->
		     extract_keywords(Name),
		     pack_index(Name)
	     end),
    init:stop().
    
%%----------------------------------------------------------------------
%% list_files(Name, [{Top, Ext}]
%%     Make a list of files with extension <Ext> 
%%     below the sub-directory <Top>
%%     Write the results to a file called <Name>.files
%%----------------------------------------------------------------------

list_files(Name, L) ->
    time_fun("listing:",
	     fun() ->
		     Out = Name ++ ".files",
		     Stream  = elib1_fast_write:new(Out),
		     Stream1 = list_files_to_stream(L, Stream),
		     {N, _}  = elib1_fast_write:close(Stream1),
		     io:format("listing: Found ~w files~n",[N]),
		     Size = filelib:file_size(Out),
		     io:format("listing: Created ~s size:~w bytes~n",
			       [Out, Size])
	     end).

list_files_to_stream([], S) ->
    S;
list_files_to_stream([{TopR,Ext}|T], Stream) ->
    Home = case os:getenv("HOME") of
	       false -> exit({noEnv,'$HOME'});
	       Str -> Str
	   end,
    Top = filename:join(Home, TopR),
    io:format("listing: a scanning from ~s~n",[Top]),
    AddFile = fun(I,J,K,L) ->
		      correct_type_of_file(I,J,K,L,Ext)
	      end,
    Stream1 = elib1_file_finder:foldl(Top, 
				      AddFile,
				      Stream, 
				      fun follow/3),
    list_files_to_stream(T, Stream1).


%% @spec correct_type_of_file(TopDir, Dir, File, Stream, Ext) -> Stream1
%% @doc  adds a FileName to Stream if the file has the extension Ext

correct_type_of_file(Top, Dir, File, Stream, Ext) ->
    case filename:extension(File) of
	Ext ->
	     FullFileName = filename:join([Top, Dir, File]),
	     elib1_fast_write:write(Stream, FullFileName);
	_ ->
	    Stream
     end.

%% Follow does not enter "hidden" directories
%%  ie those would names starts with "."

follow(_Top, _Dir, H) ->
    case H of
	"." ++ _ -> false;
	_        -> true
    end.

%%----------------------------------------------------------------------
%% end of list_files support
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------

-spec pack_files(Name::string) -> void().

%%  @doc   Input: Name.files
%%         Output:Name.crawl
%%
%%  Read all the files in Input and append together into a single
%%  file Output. Output is a steam of tuples {FileName,Md5,compress(Content)}
%%  duplicated files are not added (duplicate means has same Md5 Sum)

pack_files(Name) ->
    time_fun("pack:",
    fun() ->
	    io:format("Packing data ...~n"),
	    In = Name ++ ".files",
	    Out = Name ++ ".crawl",
	    Set = sets:new(),
	    Stream = elib1_fast_write:new(Out),
	    {_Set1, Stream1, NFiles, NBytes, _} = 
		elib1_fast_read:foldl(In, 
				      fun add_file/3, 
				      {Set,Stream,0,0,1}),
	    {NWritten, NTot} = elib1_fast_write:close(Stream1),
	    io:format("pack: ~w files (~w bytes) read~n",[NFiles,NBytes]),
	    io:format("pack: ~w files (~w bytes) written~n",[NWritten,NTot]),
	    io:format("pack: Size of input (~s): ~w bytes~n",
		      [In,filelib:file_size(In)]),
	    io:format("pack: Created (~s): ~w bytes~n",
		      [Out,filelib:file_size(Out)]),
	    if 
		NBytes > 0 ->
		    io:format("pack: Compression ratio for data = ~p~n",
			      [NTot/NBytes]);
		true ->
		    ok
	    end
    end).

add_file(File, _Pos, {Set, Stream, NRead, NBytes, Index}) ->
    {ok, Bin} = file:read_file(File),
    Md5 = erlang:md5(Bin),
    case sets:is_element(Md5, Set) of
	true -> 
	    {Set, Stream, NRead+1, NBytes, Index};
	false ->
	    %% io:format("adding:~p~n",[File]),
	    B1 = term_to_binary(Bin, [compressed]),
	    Stream1=elib1_fast_write:write(Stream,{File,"erl",Md5,B1}),
	    Set1 = sets:add_element(Md5, Set),
	    {Set1, Stream1, NRead+1, NBytes+size(Bin), Index+1}
    end.

%%----------------------------------------------------------------------
%% end of pack section
%%----------------------------------------------------------------------

extract_keywords(Name) ->
    time_fun("keywords:", fun() -> extract_keywords0(Name) end).

extract_keywords0(Name) ->
    io:format("keywords: building word table~n"),
    Word2Index = ets:new(table, [set]),
    Free = build_word_table(Word2Index),
    io:format("keywords: analysing files (can take a while) "),
    In  = Name ++ ".crawl",    
    Out = Name ++ "_unsorted.tmp", 
    OutS = elib1_fast_write:new(Out),
    %% OutS is the output stream where we will write {I,J} tuples
    {ok, result} = dets:open_file(result, 
				  {file,Name ++ ".index"}),
    {Out2, _, Free1,NFiles} = 
	elib1_fast_read:foldl(In, 
			      fun index_file/3, 
			      {OutS,Word2Index,Free,1}),
    ets:insert(Word2Index, {free, Free1}),
    elib1_fast_write:close(Out2),
    dets:close(result),
    io:format("~n"), %% because the screen is messed up
    io:format("keywords: Number of files indexed: ~w~n",[NFiles]),
    io:format("keywords: Inverting word table~n"),
    Index2Word = invert_word_table(Word2Index),
    ets:tab2file(Index2Word, Name ++ "_word2index.tmp"),
    ets:delete(Word2Index),
    ets:delete(Index2Word).

%% @doc
%% <pre>
%% index_file(Arg1, Pos, Arg2) -> Args2'
%%    Arg1  = is what we wrote to the crawl file
%%          = {Term, md5, CompressedContent}
%%    Pos   = the location of the tuple in the Input file
%%    Argg2 = {Stream, Tab,Free, FileNumber}
%%            Stream = the output stream of {I,J} integers
%%            Tab = ets table of words2index
%%            Free = first free index in word table
%%            N = FileNumber in the result table
%% </pre>

index_file({Term,Ext,_Md5,B0}, Pos, {Stream,Tab,Free,FileNumber}) ->
    Bin  = binary_to_term(B0),
    Str  = binary_to_list(Bin),
    Words = file2words(Term, Str, Ext),
    plip(FileNumber),
    {Stream1, Free1} = 
	foldl(fun(Word, {S,F}) ->
		      {Index,F1} = ensure_stored(Word, {F,Tab}),
		      S1 = elib1_fast_write:write(S, {Index,FileNumber}),
		      %% io:format("add index :~p~n",[{Index,Pos}]),
		      {S1, F1}
	      end, {Stream, Free}, Words),
    dets:insert(result, {FileNumber,{Pos,Term}}),
    {Stream1, Tab, Free1, FileNumber+1}.

%% We want to make a dets table
%%    Word -> [PagePos]

pack_index(Name) ->
    time_fun("pack index:", fun() -> pack_index0(Name) end).

pack_index0(Name) ->
    io:format("pack index: sorting~n"),
    In  =  Name ++ "_unsorted.tmp", 
    Out =  Name ++ "_sorted.tmp",
    WordTable = Name ++ "_word2index.tmp",
    file_sorter:sort([In], Out),
    io:format("pack index: sorting done~n"),
    {ok, Index2Word} = ets:file2tab(WordTable),
    NumberOfWords = ets:info(Index2Word, size),
    %% traverse the sorted word->Pos file
    %% Open a dets file for the output
    io:format("pack index: outputting index~n"),
    OutDets = Name ++ ".index",
    {ok, result} = dets:open_file(result, {file,OutDets}),
    F = fun(Tuple, _Pos, Acc) -> merge_tuples(Tuple, Acc, Index2Word) end,
    elib1_fast_read:foldl(Out, F, {-1,[]}),
    dets:close(result),
    %% Now we're done print some statistics and remove the
    %% temporary file
    io:format("pack index: Size of temporary pair map:~w bytes~n",
	      [filelib:file_size(In)]),
    file:delete(In),
    file:delete(Out),
    io:format("pack index: Size of serialised word table:~w bytes~n",
	      [filelib:file_size(WordTable)]),
    file:delete(WordTable),
    io:format("pack index: Number of words in word table:~w~n",
	      [NumberOfWords]),

    io:format("pack index: Created ~s: ~w bytes~n",
	      [OutDets, filelib:file_size(OutDets)]).
    
merge_tuples({Word,File}, {Word,L}, _Tab) ->
    %% same word
    {Word, [File|L]};
merge_tuples({New,File}, {Old, L}, Tab) ->
    case Old of 
	-1 -> void;
	_  ->
	    %% Old is the index of a word
	    %% so now we need the actual word
	    [{_,Word}] = ets:lookup(Tab, Old),
	    %% L is a list of file positions
	    %% io:format("Adding to dets:~p~n",[{Word,L}]),
	    %%
	    %% This is a check against bad logic somewhere
	    case elib1_misc:duplicates(L) of
		[] -> void;
		L2 ->
		    io:format("Bad stuff3 duplicates Word=~p ~p~n",
			      [Word,L2]),
		    exit(oops3)
	    end,
	    %% done checking 
	    L1 = lists:sort(elib1_misc:remove_duplicates(L)), 
	    Bin = elib1_gamma:alist_to_gamma(L1),
	    dets:insert(result, {Word, Bin})
    end,
    {New, [File]}.

%%----------------------------------------------------------------------
invert_word_table(Tab) ->
    %% is a ets map from Word -> Index
    %% we want Index -> Word
    %% and build an inverse map
    Tab1 = ets:new(table, [set]),
    ets:foldl(fun({Word,Index}, A) -> 
		      ets:insert(Tab1, {Index,Word}), 
		      A
	      end, 0, Tab),
    Tab1.

file2words(_Tag, Str, Ext) ->
    Mod = list_to_atom("elib1_indexer_plugin_" ++ Ext),
    Words = Mod:strings(Str),
    %% io:format("file2words Tag=~p~n~p~n",[_Tag, Words]),
    %% stem em, and remove duplicates
    Words1 = [elib1_porter:stem(I) || I <- Words],
    elib1_misc:remove_duplicates(Words1).

build_word_table(Tab) ->
    F = fun insert_english_word/2,
    {Free, _} = for_each_word_in_the_english_language(F, {1,Tab}),
    io:format("indexing: ~p words loaded~n", [Free-1]),
    ets:insert(Tab, {free, Free}),
    Free.

insert_english_word(Word, {_Free,Tab}=FT) ->
    Word1 = elib1_porter:stem(Word),
    {_Index, Free1} = ensure_stored(Word1, FT),
    {Free1, Tab}.

for_each_word_in_the_english_language(F, A0) ->
    {ok, Bin0} = file:read_file(elib1_misc:include_file("354984si.ngl.gz")),
    Bin = zlib:gunzip(Bin0),
    scan_word_list(binary_to_list(Bin), F, A0).

scan_word_list([], _, A) ->
    A;
scan_word_list(L, F, A) ->
    {Word, L1} = get_next_word(L, []),
    A1 = F(Word, A),
    scan_word_list(L1, F, A1).

get_next_word([$\r,$\n|T], L) -> {reverse(L), T};
get_next_word([H|T], L)       -> get_next_word(T, [H|L]);
get_next_word([], L)          -> {reverse(L), []}.

%%----------------------------------------------------------------------
%% ensure_stored(Word, Free, EtsTab) ->
%%  lookup word in ets table
%%     return index of word and free'
%%     If the word is already in the index then Free' = Free
%%     {Index, Free'}

ensure_stored(Word, {Free, Tab}) ->
    %% io:format("Word=~p X=~p~n",[Word,X]),
    S1 = list_to_binary(Word),
    case ets:lookup(Tab, S1) of
	[] ->
	    %% io:format("endure=~p ~n",[{S1,Free}]),
	    ets:insert(Tab, {S1, Free}),
	    {Free, Free+1};
	[{_,Index}] ->
	    {Index, Free}
    end.

%%----------------------------------------------------------------------

lookup(Name, Str) ->
    {ok, result} = 
	dets:open_file(result, 
		       {file,atom_to_list(Name) ++ ".index"}),
    Bin = list_to_binary(elib1_porter:stem(Str)),
    case dets:lookup(result, Bin) of
	[] -> [];
	[{_,Bin1}] -> 
	    elib1_gamma:gamma_to_alist(Bin1)
    end.

%%----------------------------------------------------------------------
%% query

q(Name, Str) ->
    q(".", Name, Str).

q(Dir, Name, Str) ->
    time_fun("query",
	     fun() ->
		     Dets = Dir ++ "/" ++ Name ++ ".index",
		     {ok, result} = dets:open_file(result, {file,Dets}),
		     Words = string:tokens(Str, " "),
		     L1 = [q1(I) || I <- Words],
		     Hits = [{String,N} || {String,N,_} <- L1],
		     P1 = [Pages || {_,_,Pages} <- L1],
		     L2 = intersection(P1),
		     FileNames = [filename(I) || I <- L2],
		     dets:close(result),
		     io:format("~p~n",[FileNames]),
		     {Hits, FileNames}
	     end).

-spec extract(Name::string(), I::integer()) ->
    {FileName::string(), Content::binary()}.

%% @doc given the name of a crawl file and a pointer into the
%% crawl return the filename and content of the file stored in the
%% crawl.

extract(Name, I) ->
    extract(".", Name, I).

extract(Dir, Name, I) ->
    S = elib1_fast_read:open_abs(Dir ++ "/" ++ atom_to_list(Name) ++ ".crawl"),
    Term = elib1_fast_read:read_abs(S, I),
    file:close(S),
    {FileName, _FileNumber, Md5, Ccontent} = Term,
    Content =  binary_to_term(Ccontent),
    case erlang:md5(Content) of
	Md5 ->
	    {FileName, Content};
	_ ->
	    exit({eCorruptData, FileName})
    end.

filename(I) ->
    case dets:lookup(result, I) of
	[] -> error;
	[{I,X}] -> X
    end.

q1(Str) ->
    io:format("lookup:~p~n",[Str]),
    Bin1 = list_to_binary(elib1_porter:stem(Str)),
    L = case dets:lookup(result, Bin1) of
	    [] -> [];
	    [{_,Bin2}] -> 
		elib1_gamma:gamma_to_alist(Bin2)
	end,
    io:format("~s ~p hits~n",[Str, length(L)]),
    {Str, length(L), L}.

intersection(L) ->	    
    sets:to_list(sets:intersection([sets:from_list(I) || I <- L])).

plip(N) ->
    case N rem 25 of
	0 -> io:format(".");
	_ -> void
    end,
    case N rem 100 of
	0 when N > 0 -> io:format("~w ",[N]);
	_ -> void
    end.

dump_index(Name) ->
    Dets = Name ++ ".index",    
    {ok, result} = dets:open_file(result, {file,Dets}),
    A1 = dets:foldl(fun(I, L) -> [I|L] end, [], result),
    dets:close(result),
    elib1_misc:dump("index", A1).

dump_statistics(Name) ->
    Dets = Name ++ ".index",    
    {ok, result} = dets:open_file(result, {file,Dets}),
    A1 = dets:foldl(fun analyse_entry/2, [], result),
    dets:close(result),
    N1 = lists:sum([ N || {_,N,_,_} <- A1]),
    N2 = lists:sum([ N || {_,_,N,_} <- A1]),
    N3 = lists:sum([min(NN1,NN2) || {_,NN1,NN2,_} <- A1]),
    elib1_misc:dump("statistics", 
		    {gamma,N1,term_to_binary,N2,optimal,N3,A1}).

min(A, B) when A < B -> A;
min(_, B) -> B.

analyse_entry({Key,Gamma}, L) when is_binary(Key) ->
    %% Val is a fancy thing ...		
    PageNumbers = elib1_gamma:gamma_to_alist(Gamma),
    C1 = size(term_to_binary(PageNumbers)),
    [{Key,size(Gamma),C1,PageNumbers}|L];
analyse_entry(_, L) ->
    L.

dump(Name) ->
    L = elib1_fast_read:foldl(Name, 
			      fun(T,Pos,L) -> [{Pos,T}|L] end, 
			      []),
    elib1_misc:dump(Name, reverse(L)).


%% 93> packer:all(big).
%% listing: files from:"/home/ejoearm"
%% listing: Found 15423 files
%% listing: took 142064 ms
%% Packing data ...
%% pack: 15423 files (201611576 bytes) read
%% pack: 10297 files (41888015 bytes) written
%% pack: compression = 0.20776592213137604
%% pack: took 61691 ms
%% indexing: building word table
%% indexing: 215830 words loaded
%% pack index: sorting
%% pack index: sorting done
%% pack index: outputting index
%% pack index: took 32723 ms
%% Initially 15K files (201 MB)
%% Reduced to 10K      (compressed 41 MB)
%% Size of index       12 MB)
%% Times
%%  listing            142   seconds
%%  packing             61   seconds
%%  keyword extraction 147   seconds
%%  making index        38   seconds
%%  Total              398   seconds
%%  25 files/second
    
