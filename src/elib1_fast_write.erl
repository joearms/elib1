%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(elib1_fast_write).
-export([new/1, new/2, write/2, close/1, stream_pos/1]).

%% default is to buffer 32 KB

%% new(File) -> Buffer
%% new(File,NumberOfBuffers) -> Buffer
%% write(Buffer, Term) -> Buffer'
%% close(Buff) -> {#TermsWritten,#bytesWritten}

-type stream() :: {iow, 
		   file:io_device(), 
		   integer(), 
		   integer(), 
		   integer(), 
		   integer(), 
		   [byte()]}.

%%----------------------------------------------------------------------
%% @doc Create a new stream

-spec new(FileName::string()) -> stream().

new(File) ->
    new(File, 8).

new(File, N) ->
    %% N is in units of 4096 bytes
    {ok, Stream} = file:open(File,
			     [write,binary,raw,delayed_write]),
    {iow,Stream,0,0,0,N*4096,[]}.

%% Data structure = {iow,Stream,N,W,T,Max,L}
%%    Stream = io:stream
%%    N = number of bytes in the buffer
%%    W = total bytes written
%%    T = total number of terms written
%%    Max = Max size of buffer
%%    L = Buffer

stream_pos({iow,_,_,W,_,_,_}) ->
    W+1.


%%----------------------------------------------------------------------
%% @doc write(Stream, Term) -> newStream.

-spec write(stream(), any()) -> stream().

write({iow,Stream,N,W,T,Max,L}, Term) ->
    B = term_to_binary(Term),
    Size = size(B),
    L1 = [L,<<Size:32/big>>,B],
    N1 = N + Size + 4, %% number of bytes in current buffer
    W1 = W + Size + 4,
    T1 = T + 1,
    if 
	N1 > Max ->
	    ok = file:write(Stream, L1),
	    {iow, Stream,0,W1,T1,Max,[]};
	true ->
	    {iow, Stream,N1,W1,T1,Max,L1}
    end.

%%----------------------------------------------------------------------
%% @doc Close stream

-spec close(stream()) ->
    {NumberOfTermsWritten::integer(),  NumberOfBytesWritten::integer()}.

close({iow,Stream,0,W,T,_,_}) ->
    file:close(Stream),
    {T,W};
close({iow,Stream,_,W,T,_,L}) ->
    file:write(Stream, L),
    file:close(Stream),
    {T,W}.



