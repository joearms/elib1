%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(elib1_fast_read).
-compile(export_all).
-export([foldl/3, open_abs/1, close_abs/1, read_abs/2]).

%% foldl(File, Fun, Acc) -> Acc1
%%    calls Fun(Tup, Pos, Acc) -> Acc1
%%    for each tuple in <File>
%%    File is a list of tuples encoded with
%%    <<Len:32/integer, term_to_binary(Tup)>>
%%    Pos is the *absolute* position in the file
%%    of the 4 byte header
%% open_abs(File) -> Stream
%% close_abs(Stream) -> void
%% read_abs(Stream, Pos) -> Term

%%----------------------------------------------------------------------
%% @doc Fold a fun over a file of terms.
%% The terms are stored in binary term format.
%% Fun is of the form <b>F(Term, Pos, Acc) -> NewAcc</b>
%% Terms are read one at a time from the file. Pos is an integer
%% index into the file this can be used later as an argument
%% to <b>read_abs</b>

-type acc() :: any().

-type filename() :: string().

-type stream()::file:io_device().

%% Acc is an accumulator

-spec foldl(filename(),
	    fun((any(), Pos::integer(), acc()) -> acc()),
	    acc()) -> acc().

foldl(File, Fun, Acc0) ->
    %% io:format("opening:~p~n",[File]),
    {ok, Stream} = file:open(File, [read,raw,binary,{read_ahead, 64000}]),
    try read2a(Stream, Fun, Acc0, 0, <<>>) of
	Acc1 ->
	    file:close(Stream),
	    Acc1
    catch
	throw:X ->
	    file:close(Stream),
	    X
    end.

%% there is one unit test for elib1_fast_read and elib_fast_write

foldl_test() ->
    Stream = elib1_fast_write:new("./tmp.tmp"),
    S1 = elib1_fast_write:write(Stream, {term,1}),
    S2 = elib1_fast_write:write(S1, "it works"),
    S3 = elib1_fast_write:write(S2, {term,2,more,stuff}),
    elib1_fast_write:close(S3),
    %% Now recover all the elements in the stream
    L = foldl("./tmp.tmp", fun(Term, Pos, A) -> [{Pos, Term}|A] end, []),
    [{Pos3,{term,2,more,stuff}},{Pos2,"it works"},{Pos1,{term,1}}] = L,
    Stream1 = open_abs("./tmp.tmp"),
    "it works" = read_abs(Stream1, Pos2),
    {term,1} = read_abs(Stream1, Pos1),
    {term,2,more,stuff} = read_abs(Stream1, Pos3),
    close_abs(Stream1),
    ok.

%%----------------------------------------------------------------------
%% @doc Create a stream from a file.

-spec open_abs(filename()) -> stream().

open_abs(File) ->
    {ok, Stream} = file:open(File, [read,raw,binary,{read_ahead, 64000}]),
    Stream.

%%----------------------------------------------------------------------
%% @doc Close a stream.

-spec close_abs(stream()) -> ok.

close_abs(Stream) ->
    file:close(Stream).

%%----------------------------------------------------------------------
%% @doc Read a term given the absolute position from a stream.

-spec read_abs(stream(), Pos::integer()) -> any().

read_abs(Stream, Pos) ->
    {ok, <<I:32/big>>} = file:pread(Stream, Pos, 4),
    {ok, Bin} = file:pread(Stream, Pos+4, I),
    binary_to_term(Bin).

read2a(Stream, Fun, A0, Pos, <<Size:32/big,B/binary>>=Bx) ->
    case size(B) of
	N when N >= Size ->
	    {Ba, Bb} = split_binary(B, Size),
	    T = binary_to_term(Ba),
	    %% This is where we call the user's function
	    A1 = Fun(T, Pos, A0),
	    read2a(Stream, Fun, A1, Pos + Size + 4, Bb);
	_ ->
	    case
		file:read(Stream, 64000) of
		{ok, Bc} ->
		    read2a(Stream, Fun, A0, Pos, <<Bx/binary, Bc/binary>>);
		eof ->
		    A0
	    end
    end;
read2a(Stream, Fun, A0, Pos, Bx) ->
        case
	    file:read(Stream, 64000) of
	    {ok, Bc} ->
		read2a(Stream, Fun, A0, Pos, <<Bx/binary, Bc/binary>>);
	    eof ->
		A0
	end.


