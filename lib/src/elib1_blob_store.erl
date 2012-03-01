%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(elib1_blob_store).

%% The guid store is a two level store
%%
%% M:init(File)
%% M:store(Key, Blob)
%% M:fetch(Key) -> Blob raises eNoKey
%% M:keys() -> [Key]

-export([open/1, close/0, fetch/1, store/2, keys/0]).

open(File) ->
    %% io:format("dets opened:~p~n", [File]),
    case dets:open_file(?MODULE, [{file, File}]) of
	{ok, ?MODULE} ->
	    true;
	{error,_Reason} ->
	    io:format("cannot open dets table~n"),
	    exit(eDetsOpen)
    end.

close() -> dets:close(?MODULE).

store(Key, Blob) when is_binary(Blob) ->
    %% io:format("storing blob key=~p size=~p~n",[Key,size(Blob)]),
    ok = dets:insert(?MODULE, [{Key,Blob}]).

fetch(Key) ->
    case dets:lookup(?MODULE, Key) of
	[]         -> error;
	[{_,Blob}] -> {ok, Blob}
    end.

keys() ->
    %% I guess there is a better way of doing this ...
    dets:foldl(fun({K,_},A) -> [K|A] end,[],?MODULE).

		       
    
			   
