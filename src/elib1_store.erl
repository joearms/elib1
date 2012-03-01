%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(elib1_store).

%% -compile(export_all).
%% Time-stamp: <2011-02-06 16:58:08 joe>

%% The GLOB Store 

%% M:create_store(FileName, N   ) - fails if the store is running
%%                                  build a store with hash table size 2^N
%% M:start(FileName)              - opens the store
%% M:stop()                       - stops the store
%% M:store(Key, Bin) -> Vsn       - store a K,Bin pair
%% M:lookup(Key) -> {ok, {Vsn,Prev,Term}} | error
%%    Prev = {0,0} if there is no previous
%% M:lookup(Key, Vsn) -> same return as above
%% M:lookup_abs(Prev) -> {ok,{Prev',Val}}
%% M:fold_values(Key, Fun, Acc) 
%%   applies Acc' = Fun(Key, Vsn, Val, Acc) to each value of the key

-import(lists, [min/1, duplicate/2, reverse/1]).

-import(file, [pwrite/3]).

-export([test/0, 
	 create_store/2,
	 all_keys/0,
	 all_old_values/1,
	 fold_values/3,
	 start/1, 
	 lookup/1, 
	 lookup/2, 
	 lookup_abs/1,
	 store/2, 
	 stop/0]).

test() ->
    (catch exit(whereis(?MODULE), kill)),
    file:delete("foo"),
    create_store("foo", 3),
    start("foo"),
    1 = store(joe, {a,b,12,{d,e}}),
    1 = store(fred, [1,2]),
    {ok, {1,P1,{a,b,12,{d,e}}}} = lookup(joe),
    io:format("P1=~p~n",[P1]),
    2 = store(joe, [a,list,{with,a,tuple}]),
    {ok, {2,P2,[a,list,{with,a,tuple}]}} = lookup(joe),
    io:format("P2=~p~n",[P2]),
    3 = store(joe, [yes,another,thing]),
    {ok, {3,Prev,[yes,another,thing]}} = lookup(joe),
    {ok, {Prev1, [a,list,{with,a,tuple}]}} = 
	lookup_abs(Prev),
    io:format("Prev1=~p~n",[Prev1]),
    {ok, {Prev2, {a,b,12,{d,e}}}} = lookup_abs(Prev1),
    {0,0} = Prev2,
    error = lookup(joe,4),
    {ok,{3,_,[yes,another,thing]}} = lookup(joe,3),
    {ok,{2,_,[a,list,{with,a,tuple}]}} = lookup(joe,2),
    {ok,{1,_,{a,b,12,{d,e}}}} = lookup(joe,1),
    {ok,{3,_,[yes,another,thing]}} = lookup(joe),
    error = lookup(silly),
    %% lookup(joe).
    1 = store(james, {james,the,3,rf}),
    1 = store({a,dict}, {james,the,3,rf}),
    1 = store(collide, "will we collide 0"),
    2 = store(collide, "more collisions 1"),
    2 = store({a,dict}, "dict vsn 1"),
    debug(),
    Keys = all_keys(),
    io:format("Keys=~p~n",[Keys]),
    lists:foreach(fun(I) -> examine_key1(I) end, Keys),
    lists:foreach(fun(I) -> examine_key2(I) end, Keys),
    error = lookup(joe,7),
    worked.

examine_key2(Key) ->
    Vals = all_old_values(Key),
    io:format("Key=~p OldValues=~p~n",[Key,Vals]).

all_old_values(Key) ->
    fold_values(Key, fun(_,N,Val,A) -> [{N,Val}|A] end, []).

examine_key1(Key) ->
    L = fold_values(Key, 
		    fun(K,N,Val,A) ->
			    [{K,N,Val}|A]
		    end, []),
    io:format("~p~n",[L]).

fold_values(Key, Fun, Acc) ->
    case lookup(Key) of
	error -> [];
	{ok, {Max,Prev,Val}} ->
	    Acc1 = Fun(Key, Max, Val, Acc),
	    fold_values(Prev, Key, Max-1, Fun, Acc1)
    end.

fold_values({0,0}, _, _, _, A)  -> A;
fold_values(Prev, Key, N, F, A) ->
    {ok, {Prev1,Val}} = lookup_abs(Prev),
    A1 = F(Key, N, Val, A),
    fold_values(Prev1, Key, N-1, F, A1).

store(K, V) ->  rpc({do_store,K,V}).

debug() -> cast(debug).

all_keys() ->
    rpc(all_keys).

lookup(Key,Vsn) when is_integer(Vsn), Vsn > 0 ->  
    rpc({lookup,Key,Vsn}).
    
lookup(K) -> rpc({lookup, K, last}).

stop() -> rpc(stop).

lookup_abs(Prev) ->
    rpc({lookupPrev, Prev}).

rpc(Q) ->
    ?MODULE ! {self(), Q},
    receive
	{?MODULE, R} ->
	    %% io:format("Rpc Q=~p R=~p~n",[Q,R]),
	    R
    end.

cast(X) ->
    ?MODULE ! X.

%% create a new store File is the filename for the store
%% Bits is the number of bits in the hash table
%% So Bits = 3 will create a hash table of length 8
%% This opens and closes a new file

create_store(File, Bits) ->
    case whereis(?MODULE) of
	undefined ->
	    create_store1(File, Bits);
	_ ->
	    io:format("Store is running - stop the store and retry~n")
    end.

create_store1(File, Bits) ->
    case filelib:is_file(File) of
	true ->
	    io:format("A Store in:~s exists"
		      "change the name of the store or delete this file~n",
		      [File]),
	    error;
	false ->
	    {ok, S} = file:open(File, [read,write,raw,binary]),
	    Vsn = 1,
	    MaxHash = 2 bsl (Bits-1),
	    io:format("Hash table size = ~p~n",[MaxHash]),
	    %% write a version - the max value of the hash
	    %% then MaxHash 32 bit words
	    pwrite(S, 0, [<<Vsn:32, MaxHash:32>>]),
	    zero_buffer(S, 8, 8*MaxHash),
	    file:close(S),
	    io:format("Store:~s created~n",[File])
    end.

%% zero_buffer(FileHandle, Start, Len)
%%    writes Len zeros from adress Start in a file
%%    The units of Start and Len are in Bytes
%%    Start = 1 is the first byte in the file (check)

zero_buffer(S, Start, BuffLen) ->
    N = min([BuffLen, 4096]),
    B1 = list_to_binary(duplicate(N, 0)),
    zero_buffer(S, Start, BuffLen, B1, N).

zero_buffer(S, Start, Left, B, Size) when Size < Left ->
    pwrite(S, Start, B),
    zero_buffer(S, Start+Size, Left-Size, B, Size);
zero_buffer(S, Start, Left, _, _) ->
    B = list_to_binary(duplicate(Left, 0)),
    pwrite(S, Start, B).
    
start(File) ->
    register(?MODULE, 
	     spawn_link(fun() -> start1(File) end)).

start1(File) ->
    {ok, S} = file:open(File, [read,write,raw,binary]),
    Len = filelib:file_size(File),
    {ok, <<1:32,HashTableSize:32>>} = file:pread(S,0,8),
    %% io:format("# hash size=~p~n",[HashTableSize]),
    loop(S, Len, HashTableSize).

loop(S, Len, HashTableSize) ->
    receive
	{From, all_keys} ->
	    From ! {?MODULE, disk_all_keys(S)},
	    loop(S, Len, HashTableSize);
	{From, stop} ->
	    file:close(S),
	    From ! {?MODULE, ack};
	debug ->
	    debug(S),
	    loop(S, Len, HashTableSize);
	{From, {lookupPrev, Prev}} ->
	    Reply = disk_lookup_previous(S, Prev, Len),
	    From ! {?MODULE, Reply},
	    loop(S, Len, HashTableSize);
	{From, {do_store, Key, Val}} ->
	    {Vsn, Len1} = disk_store(S, Key, Val, HashTableSize, Len),
	    From ! {?MODULE, Vsn},
	    loop(S, Len1, HashTableSize);
	{From, {lookup, Key, Vsn}} ->
	    Reply = disk_lookup(S, Key, HashTableSize, Vsn),
	    From ! {?MODULE, Reply},
	    loop(S, Len, HashTableSize)
    end.

disk_lookup_previous(S, {Ptr,N}, Len) when is_integer(Ptr),
                                           is_integer(N),
                                           Ptr > 0, Ptr < Len,
                                           N > 0, Ptr + N =< Len ->
    {P,L,Val} = read_value(S, Ptr, N),    
    {ok, {{P,L}, Val}};
disk_lookup_previous(_, _, _) ->
    error.

disk_lookup(S, Key, HashTableSize, RequiredVsn) ->
    Index = erlang:phash(Key, HashTableSize),
    %% io:format("Hash Index=~p~n",[Index]),
    Loc = 8*Index + 8,
    {Ptr, KLen} = read_2_words(S, Loc),
    case Ptr of
	0 ->
	    error;
	_ ->
	    case find_key(S, term_to_binary(Key), Ptr, KLen) of
		{found, _Ptr, Vsn, ValPtr, ValLen} ->
		    locate_value(S, ValPtr, ValLen, Vsn, RequiredVsn);
		{nokey,_} ->
		    error
	    end
    end.

locate_value(S, ValPtr, ValLen, Vsn, last) ->
    {P,L,Val} = read_value(S, ValPtr, ValLen),    
    {ok, {Vsn, {P,L}, Val}};
locate_value(S, ValPtr, ValLen, Vsn, Vsn) ->
    {P,L,Val} = read_value(S, ValPtr, ValLen),    
    {ok, {Vsn, {P,L}, Val}};
locate_value(S, ValPtr, ValLen, Vsn, Req) ->
    {P,L,_} = read_value(S, ValPtr, ValLen),    
    case P of
	0 -> error;
	_ -> locate_value(S, P, L, Vsn-1, Req)
    end.

disk_store(S, Key, Val, HashTableSize, Len) -> 
    Index = erlang:phash(Key, HashTableSize),
    %% io:format("Hash Index=~p~n",[Index]),
    %% read a pointer and keylength from the hash table
    Loc = 8*Index + 8,
    {Ptr, KLen} = read_2_words(S, Loc),
    case Ptr of
	0 ->
	    %% there is no pointer
	    {Len1,KeyLen} = insert_initial_keyval(S, Key, Val, Len),
	    %% update # table
	    pwrite(S, Loc, <<Len:32, KeyLen:32>>),
	    {1, Len1};
	_ ->
	    case find_key(S, term_to_binary(Key), Ptr, KLen) of
		{found, Ptr1, Vsn, Prev, ValLen} ->
		    Size = 
			write_new_val(S, Val, Len, Prev, ValLen),
		    Len1 = Len + Size,
		    Vsn1 = Vsn + 1,
		    pwrite(S, Ptr1, <<Vsn1:32, Len:32, Size:32>>),
		    {Vsn1, Len1};
		{nokey, InsertionPointer} ->
		    {Len1,KeyBlockSize} = insert_initial_keyval(S, Key, 
							     Val, Len),
		    pwrite(S, InsertionPointer, 
			   <<Len:32, KeyBlockSize:32>>),
		    {1, Len1}
	    end
    end.

write_new_val(S, Val, Len, Prev, PrevLen) ->
    %% write a new val at the end of the file
    B = term_to_binary(Val),
    pwrite(S, Len, <<Prev:32, PrevLen:32, B/binary>>),
    size(B) + 8.

find_key(S, BKey, Ptr, Len) ->
    {ok, Bin} = file:pread(S, Ptr, Len),
    <<PNextKey:32,NextKeyLen:32,Vsn:32,PVal:32,LenVal:32,B/binary>> = Bin,
    case B of
	BKey ->
	    %% found the key
	    {found, Ptr+8, Vsn, PVal, LenVal};
	_ ->
	    %% wrong key
	    if
		PNextKey =:= 0 ->
		    %% no next key
		    %% return the insertion point
		    %% for a new key
		    {nokey, Ptr};
		true ->
		    %% go try the next key
		    find_key(S, BKey, PNextKey, NextKeyLen)
	    end
    end.

read_hash_bucket(S, I) ->
    read_2_words(S, 8*I + 8).

read_2_words(S, Index) ->
    {ok, <<I:32,J:32>>} = file:pread(S, Index, 8),
    %% io:format("read bucket[~p]=~p#~p~n",[Index, Ptr,KLen]),
    {I, J}.

%%----------------------------------------------------------------------
%% insert_initial_keyval(S, Key, Val, Len) -> Len'
%%    Len is the length of the file descriptor
%%    this is used the first time a key and value are inserted
%%    in this case we assemble the key and value into a single block
%%

%% Keybocks
%%  <<PNext:32,   Pointer to the next next Key with the same #
%%    LNext:32,   length of the next keyblock
%%    Vsn:32,     Max version of the Key (starts at 1)
%%    PVal:32,    Pointer to the value
%%    LVal:32,    Length of the value
%%    B/binary>> = Bin,

insert_initial_keyval(S, Key, Val, Len) ->
    BKey = term_to_binary(Key),
    BVal = term_to_binary(Val),
    KeyBlockSize = 20 + size(BKey),
    Len1 = Len + KeyBlockSize,
    ValLen = size(BVal)+8,
    Block = list_to_binary([<<0:32,0:32,1:32,
			     Len1:32,ValLen:32>>,
			    BKey,
			    <<0:32,0:32>>,
			    BVal]),
    Size = size(Block),
    pwrite(S, Len, Block),
    Len2 = Len + Size,
    {Len2, KeyBlockSize}.


disk_all_keys(S) ->
    {ok, <<1:32,Size:32>>} = file:pread(S,0,8),
    io:format("debug max index=~p~n",[Size]),
    L = collect_keys(S, 1, Size, []),
    lists:usort(L).

collect_keys(S, Max, Max, L) ->
    add_keys(S, Max, L);
collect_keys(S, I, Max, L) ->
    L1 = add_keys(S, I, L),
    collect_keys(S, I+1, Max, L1).

add_keys(S, I, L) ->
    case read_hash_bucket(S, I) of
	{0,_}      -> L;
	{Ptr, Len} -> add_key_chain(S, Ptr, Len, L)
    end.
    
add_key_chain(_, 0, 0, L) -> L;
add_key_chain(S, Ptr, Len, L) ->
    {ok, Bin} = file:pread(S, Ptr, Len),
    <<PNext:32,LNext:32,_Vsn:32,_PVal:32,_LVal:32,B/binary>> = Bin,
    Key = binary_to_term(B),
    add_key_chain(S, PNext, LNext, [Key|L]).



%%----------------------------------------------------------------------
%% print the entire table

debug(S) ->
    io:format("DEBUG starting ...~n++++++++++++++++++++++++++++++~n"),
    {ok, <<1:32,Size:32>>} = file:pread(S,0,8),
    io:format("debug max index=~p~n",[Size]),
    for(1,Size,
	fun(I) ->
		case read_hash_bucket(S, I) of
		    {0,_} -> void;
		    {Ptr, Len} ->
			io:format("hash[~p]=~p#~p~n",[I,Ptr,Len]),
			print_keys(S, Ptr, Len)
		end
	end),
    io:format("---------------------------------~n").

print_keys(_, 0, 0) -> void;
print_keys(S, Ptr, Len) ->
    {ok, Bin} = file:pread(S, Ptr, Len),
    %% io:format("Key ~p#~p Bin=~p~n",[Ptr,Len,Bin]),
    <<PNext:32,LNext:32,Vsn:32,PVal:32,LVal:32,B/binary>> = Bin,
    Key = binary_to_term(B),
    io:format("Key ~p#~p (next key=~p#~p) Vsn=~p Key=~p  (Val=~p#~p)~n",
	      [Ptr,Len,PNext,LNext,Vsn,Key, PVal,LVal]),
    print_value_chain(S, Vsn, PVal, LVal),
    print_keys(S, PNext, LNext).

print_value_chain(_, _, 0, 0) -> void;
print_value_chain(S, Vsn, Ptr, Len) ->
    {PNext, LenNext, Val} = read_value(S, Ptr, Len),
    io:format("Value: ~p#~p (next=~p#~p) Vsn:~p Val=~p~n",
	      [Ptr, Len, PNext,LenNext, Vsn, Val]),
    print_value_chain(S, Vsn-1, PNext, LenNext).

%%----------------------------------------------------------------------

read_value(S, Ptr, Len) ->
    {ok, Bin} = file:pread(S, Ptr, Len),
    <<PNext:32,LenNext:32,B/binary>> = Bin,
    Val = binary_to_term(B),
    {PNext, LenNext, Val}.

for(N, N, F) -> F(N);
for(I, N, F) -> F(I),for(I+1, N, F).

			
		    
