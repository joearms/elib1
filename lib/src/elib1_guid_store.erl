%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(elib1_guid_store).

%% The entire history of a file is kept in an association list
%% as a set of patches

-export([test/0, store/1, fetch/1, fetch/2]).
-import(lists, [reverse/1, reverse/2, filter/2, member/2]).

test() ->
    file:delete("myblob.dets"),
    elib1_blob_store:open("myblob.dets"),
    store([{guid,g1},{name,joe},{content,"abc"}]),
    store([{guid,g1},{name,joe1},{content,"abc\ndef"}]),
    store([{guid,g1},{date,123},{content,"123"}]),
    store([{guid,g1},{date,125},{stuff,123}, {content,"123abc\ndef\n\234"}]),
    store([{guid,g1},{nonsense,foodedo}, {content,""}]),
    store([{guid,g1},{nonsense,foodedo12313}, {content,"123123"}]),
    elib1_blob_store:close().

fetch(Guid) -> fetch(Guid, 0).

fetch(Guid, N) ->
    case elib1_blob_store:fetch(Guid) of
	{ok, OldBlob} ->
	    {Meta, Patches} = binary_to_term(OldBlob),
	    fetch1(Meta, N, Patches);
	error ->
	    exit(eBadGuid)
    end.

fetch1(Meta, 0, _) -> Meta;
fetch1(Meta, N, [P|T]) ->
    OldMeta = patch(P, Meta),
    fetch1(OldMeta, N-1, T);
fetch1(_, N, []) ->
    exit({ebadLevel,N}).

store(Assoc) ->
    Guid = must(guid, Assoc),
    NewBlob = case elib1_blob_store:fetch(Guid) of
		  {ok, OldBlob} ->
		      {OldMeta, Patches} = binary_to_term(OldBlob),
		      P = diff(Assoc, OldMeta),
		      term_to_binary({Assoc,[P|Patches]});
		  error ->
		      P = diff(Assoc, [{content,""}]),
		      term_to_binary({Assoc,[P]})

	      end,
    elib1_blob_store:store(Guid, NewBlob).

%% patch(P, NewMeta) -> OldMeta'

patch([{k,K,V}|T], Meta) ->
    %% key replace
    Meta1 = replace(K, V, Meta, []),
    patch(T, Meta1);
patch([{d,K}|T], Meta) ->
    Meta1 = delete_key(K, Meta),
    patch(T, Meta1);
patch([{p,P}|T], Meta) ->
    NewContent = must(content, Meta),
    OldContent = elib1_diff:patch(NewContent, P),
    Meta1 = replace(content, OldContent, Meta, []),
    patch(T, Meta1);
patch([], Meta) ->
    lists:sort(Meta).

%% diff(NewMeta, OldMeta) -> Patches
%%   compute the diffs necessary to turn the current metadata and content
%%   into the old metadata and content

diff(NewMeta, OldMeta) ->
    Patch = diff1(NewMeta, OldMeta),
    %% io:format("Patches=~p~n",[Patch]),
    Old = patch(Patch, NewMeta),
    case {lists:sort(Old),
	  lists:sort(OldMeta)} of
	{A, A} ->
	    io:format("patches ok ...~n"),
	    Patch;
	_ ->
	    io:format("oops debug me"),
	    elib1_misc:dump("debug",{newMeta,lists:sort(NewMeta),
				     oldMeta,lists:sort(OldMeta),
				     patch, Patch,
				     old, lists:sort(Old)}),
	    exit(oops)
    end.

%% comput a patch to turn New into Old

diff1(NewMeta, OldMeta) ->
    L1 = deletions(NewMeta, OldMeta),
    diff(OldMeta, NewMeta, L1).

deletions(New, Old) ->
    %% keys in new that are just not in Old
    NewKeys = [K || {K,_} <- New],
    OldKeys = [K || {K,_} <- Old],
    L = filter(fun(I) -> not member(I, OldKeys) end, NewKeys),
    [{d,I} || I <- L].

diff([{content,OldContent}|T], New, L) ->
    NewContent = must(content, New),
    P = elib1_diff:diff(OldContent, NewContent),
    diff(T, New, [{p,P}|L]);
diff([{K,V}|T], New, L) ->
    case lookup(K, New) of
	{value, V} ->
	    %% same value in both no change
	    diff(T, New, L);
	_ ->
	    %% not the same value or missing
	    diff(T, New, [{k,K,V}|L])
    end;
diff([], _, L) ->
    L.

lookup(Key, L) ->
    case lists:keysearch(Key, 1, L) of
	{value, {_,V}} -> {value, V};
	false  -> false
    end.


must(K, [{K,V}|_]) -> V;
must(K, [_|T])     -> must(K, T);
must(K, [])        -> exit({eMissingkey, K}).

replace(K, V, [{K,_}|T], L) -> reverse(L, [{K,V}|T]);
replace(K, V, [H|T], L)     -> replace(K, V, T, [H|L]);
replace(K, V, [], L)        -> [{K,V}|L].

delete_key(K, [{K,_}|T]) -> T;
delete_key(K, [H|T])     -> [H|delete_key(K, T)];
delete_key(_, [])        -> [].



