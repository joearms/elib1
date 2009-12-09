%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(elib1_similar).

-export([batch_find_similar/1, find_similar_files/1]).

%% -compile(export_all).

-import(lists, [foreach/2, map/2, member/2, filter/2, 
		reverse/1, sort/1]).

-import(elib1_misc, [time_fun/2]).

%% test()  -- builds a hash table
%% sims(K) -- finds at most (K)similar files
%% c(similar_file_finder_new),similar_file_finder_new:test1().

%%----------------------------------------------------------------------
%% Make_hashes(Trawl) -> hashes

make_hashes(Trawl) ->
    Ets = ets:new(table, [bag]),
    elib1_fast_read:foldl(Trawl, fun add_hash/3, Ets),
    Out = filename:rootname(Trawl) ++ ".hashes",
    ets:tab2file(Ets, Out),
    ets:delete(Ets),
    ok.

add_hash({_FileName,_Index,_Md5,Cbin}, Pos, Ets) -> 
    Bin = binary_to_term(Cbin),
    H = add_hashes(Bin),
    %% io:format("File:~p size:~p Pos=~p Hashes=~p~n",[FileName,size(Bin),Pos, H]),
    [ets:insert(Ets,{Hash,Pos}) || Hash <- H], 
    Ets.

add_hashes(B) ->
    S = size(B),
    case gap(S) of
	no -> [];
	L  -> 
	    [block_checksum_bin(B, I, I+299) || I <- L]
    end.

%%----------------------------------------------------------------------
%% find_similar_files(Stem)
%%    Stem.trawl -> Stem.hashes x Stem.fsim

batch_find_similar([A]) ->
    time_fun("total time to find similar files",
	     fun() ->
		     find_similar_files(atom_to_list(A))
	    end),
    init:stop().



find_similar_files(Base) ->
    Crawl  = Base ++ ".crawl",
    Hashed = Base ++ ".hashes",
    Out = Base ++ ".fsim",
    make_hashes(Crawl),
    {ok, Hash} = ets:file2tab(Hashed),
    CrawlStream = elib1_fast_read:open_abs(Crawl),
    {_,_,L} = 
	elib1_fast_read:foldl(Crawl, fun find_sim/3, {Hash, CrawlStream, []}),
    ets:delete(Hash),
    elib1_fast_read:close_abs(CrawlStream),
    {ok,S} = file:open(Out, [write]),
    io:format(S, "~p.~n", [L]),
    file:close(S),
    io:format("Created:~s~n",[Out]).

find_sim({FileName,_Index,_Md5,Cbin}, Pos, {Ets, S, L}) -> 
    Bin = binary_to_term(Cbin),
    Sim = find_similar(Bin, 300, Ets),
    io:format("Pos=~p FileName=~p~n",[Pos, FileName]),
    %% Sim = [Pos] ie FilePositions
    %% Sim includes ourself
    Sim1 = [P || P <- Sim, P =/= Pos],
    %% io:format("sim1:~p~n",[Sim1]),
    %% Sim1 = [I] I can occure several times
    %% So we want to frequency count I,s
    Freq = elib1_misc:list2frequency_distribution(Sim1),
    %% io:format("freqs:~p~n",[Freq]),
    %% Order by second argument
    Freq1 = reverse(lists:keysort(2, Freq)),
    %% io:format("freq1:~p~n",[Freq1]),
    %% Take the top ten files and remove the frequency count
    Candidates = [N || {N,_Count} <- lists:sublist(Freq1, 10)],
    %% io:format("Candidates:~p~n",[Candidates]),
    Sims = sim_index(Bin, Candidates, S),
    L1 = [{FileName,Pos, Sims}|L],
    {Ets, S, L1}.

sim_index(Bin, PosL, S) ->
    Md5Sums1 = md5_sums_str(binary_to_list(Bin)),
    Sims1 = [sim1(Md5Sums1, I, S) || I <- PosL], 
    %% Sims1 = [{File,Pos}, Sim}]
    %% Sim is a number from 0-1
    %% io:format("Sims1=~p~n",[Sims1]),
    Thresh = 0.05, %% threshold for accepting a file
    Sims2 = [{I,J} || {I,J} <- Sims1, J > Thresh],
    %% Now order with the most similar first
    Sims3 = reverse(lists:keysort(2, Sims2)),
    %% io:format("Sims3=~p~n",[Sims3]),
    Sims3.
    
sim1(Md5Sums1, Pos, Stream) ->
    Term = elib1_fast_read:read_abs(Stream, Pos),
    {File, _Index, _Md5, CContent} = Term,
    Bin1 = binary_to_term(CContent),
    Str1 = binary_to_list(Bin1),
    Md5Sums2 = md5_sums_str(Str1),
    Similaratity = sorensen(Md5Sums1, Md5Sums2),
    {{File,Pos}, Similaratity}.
			  
find_similar(Bin, BlockSize, Tab) ->
    Max = size(Bin),
    if
	BlockSize < Max ->
	    %% We want to share data :-)
	    All    = binary_to_list(Bin),
	    Tail   = lists:nthtail(BlockSize, All),
	    First  = lists:sublist(All, BlockSize), 
	    {A, B} = block_checksum_str(First),
	    slide_window(A, B, All, Tail, Tab, []);
	true ->
	    []
    end.

%% A and B are the two rolling checksums
%% All = All the buffer
%% Tail = the tail of the buffer
%% To move forwards we add a character from the Tail
%% And remove the character in All

slide_window(_A, _B, _All, [], _Tab, L) -> L;
slide_window(A, B, [OldChar|All], [NewChar|Tail], Tab, L) ->
    L1 = case ets:lookup(Tab, {A, B}) of
	     [] -> L;
	     L2 ->
		 %% L2 is a list of {Hash,Pos} tuples but we are only 
		 %% interested in Pos
		 PosL = [Pos || {_Hash,Pos} <- L2],
		 PosL ++ L
	 end,
    %% update the rolling checksum
    A1 = A + NewChar - OldChar,
    B1 = B bxor NewChar bxor OldChar,
    slide_window(A1,B1,All,Tail,Tab,L1).


%%----------------------------------------------------------------------
%% Compute the checksum of the binary Bin between Start and Stop

block_checksum_bin(Bin, Start, Stop) ->
    B = elib1_misc:sub_binary(Bin, Start, Stop),
    block_checksum_str(binary_to_list(B)).

block_checksum_str(Str) -> block_checksum_str(Str, 0, 0).

block_checksum_str([H|T], A, B) -> block_checksum_str(T, H + A, H bxor B);
block_checksum_str([], A, B)    -> {A, B}.

%% N-300-N-300-N       2N + 600
%% N-300-N-300-N-300-N 4N + 900

gap(S) when S < 603 -> no;
gap(S) when S >= 603, S =< 904 -> 
    N = (S-600) div 3,
    [N,2*N+300];
gap(S) ->
    N = (S-900) div 4,
    [N,2*N+300,3*N+600].

%% Given a string containing a number of lines 
%% return a list of md5 sums of each line

md5_sums_str(Str) ->
    Lines = elib1_misc:string2lines(Str),
    Lines1 = filter(fun("\n") -> false;
		       ("%\n") -> false;
		       ("%%\n") -> false;
		       (_) -> true
		    end, Lines),
    [erlang:md5(I) || I <- Lines1].

%% compute Sörensen similarity index

sorensen(A, B) ->
    C = length(list_intersection(A, B)),
    2*C/(length(A) + length(B)).

list_intersection(A, B) ->
    filter(fun(X) -> member(X, B) end, A).
		   


	     

    
    
