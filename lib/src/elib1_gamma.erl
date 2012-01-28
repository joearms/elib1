%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(elib1_gamma).
%% -compile(export_all).

-import(lists, [reverse/1]).

-export([unit_test/0, dlist/1, decode_f/1,
	 alist_to_gamma/1, gamma_to_alist/1,
	 encode/1, decode/1, encode_seq/1, decode_seq/1]).

%% to encode X
%% Take unary code for 1 + floor(log2(X))
%% ++ binary of x - pow(2, floor(log2(X))) in binary length

%% "consider x = 9 floor(log2(9)) = 3
%% so 4 = 1 + 3 is coded as unary = 1110
%% followed by 9-8 =1 as a three bit binary code = 011
%% finally 1110 001"


%% Gamma codes (page 117)

%% 1 0
%% 2 100
%% 3 101
%% 4 11000
%% 5 11001
%% 6 11010
%% 7 11011
%% 8 1110000
%% 9  1110001
%% 10 1110010

unit_test() ->
    %% page 118
    test1(1,<<2#0:1>>),
    test1(2,<<2#100:3>>),
    test1(3,<<2#101:3>>),
    test1(4,<<2#11000:5>>),
    test1(5,<<2#11001:5>>),
    test1(6,<<2#11010:5>>),
    test1(7,<<2#11011:5>>),
    test1(7,<<2#11011:5>>),
    test1(8,<<2#1110000:7>>),
    test1(9,<<2#1110001:7>>),
    test1(10,<<2#1110010:7>>),
    [test2(I) || I <- lists:seq(1,pow(2,19)-1)],
    test3(1, 1),
    alist_to_gamma([1,2,3,4,12,15,18]),
    alist_to_gamma([2,4,123,44456]),
    ok.

test1(N, B) ->
    B1 = encode(N),
    case B1 of
	B ->
	    ok;
	_ ->
	    io:format("error:~p should be:~p is:~p~n",[N,B,B1]),
	    exit(1)
    end.

test2(I) when I > 0 ->
    B = encode(I),
    {I, _, _} = decode(B);
test2(_) ->
    true.

test3(65, _) ->
    true;
test3(N, A) ->
    test2(A),
    test2(A + 1),
    test2(A - 1),
    test2(A - 10),
    test2(A - 10),
    test3(N+1, 2*A).


%%----------------------------------------------------------------------
%% encode(Int) -> bits()

-spec encode(integer()) -> bitstring().

encode(X) ->
    {I,Pow} = f(X),
    B1 = unary(I+1),
    <<B1/bits,(X-Pow):I>>.

unary(1) -> <<0:1>>;
unary(N) -> B = unary(N-1),<<1:1,B/bits>>.

%% f(X) = floor(log2(X)).
%%   ie the smallest N for which 2^N > X
%%   example f(9)
%%      2^3 = 8, 2^4 = 16
%%      so 2^3.xxx = 9
%%      so X = 3.xxxx and floor(X) = 3
%%      and f(8) = {3,8}

f(X) -> f(X, 1, 0).

f(X, X, N) -> {N,X};
f(X, Acc, N) ->
    Acc1 = Acc*2,
    if
	Acc1 > X -> {N, Acc};
	true     -> f(X, Acc1, N+1)
    end.

%% encode - end
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% decode(B:bits()) -> {Int, Length, B1:bits()}

-spec decode(B::bitstring()) ->
    {Int::integer(), Length::integer(), B1::bitstring()}.

%%   Remove a gamma encoded Int from the head of the bitstring B.
%%   Return the Integer, the number of bits and the remainder of the
%%   bitstring.

%% decode(B) works just like decode

decode(B) -> decode1(B, 0, 1).

decode1(<<2#1:1,B/bits>>, N, A) -> decode1(B, N+1, 2*A);
decode1(<<2#0:1,B/bits>>, N, A) -> decode2(N, B, A).

decode2(0, B, _) -> {1,1,B};
decode2(N, B, A) ->
    <<R:N,B1/bits>> = B,
    {A+R, 2*N+1, B1}.

%% decode f is a "clearer fast version"

decode_f(<<2#0:1,B/bits>>)     -> {1,1,B};
decode_f(<<2#10:2,R:1,B/bits>>) -> {2+R,3,B};
decode_f(<<2#110:3,R:2,B/bits>>) -> {4+R,5,B};
decode_f(<<2#1110:4,R:3,B/bits>>) -> {8+R,7,B};
decode_f(<<2#11110:5,R:4,B/bits>>) -> {16+R,9,B};
decode_f(<<2#111110:6,R:5,B/bits>>) -> {32+R,11,B};
decode_f(<<2#1111110:7,R:6,B/bits>>) -> {64+R,13,B};
decode_f(<<2#11111110:8,R:7,B/bits>>) -> {128+R,15,B};
decode_f(<<2#111111110:9,R:8,B/bits>>) -> {256+R,17,B};
decode_f(<<2#1111111110:10,R:9,B/bits>>) -> {512+R,19,B};
decode_f(<<2#11111111110:11,R:10,B/bits>>) -> {1024+R,21,B};
decode_f(<<2#111111111110:12,R:11,B/bits>>) -> {2048+R,23,B};
decode_f(<<2#1111111111110:13,R:12,B/bits>>) -> {4096+R,25,B};
decode_f(<<2#11111111111110:14,R:13,B/bits>>) -> {8192+R,27,B};
decode_f(<<2#111111111111110:15,R:14,B/bits>>) -> {16384+R,29,B};
decode_f(<<2#1111111111111110:16,R:15,B/bits>>) -> {32768+R,31,B};
decode_f(<<2#11111111111111110:17,R:16,B/bits>>) -> {65536+R,33,B};
decode_f(<<2#111111111111111110:18,R:17,B/bits>>) -> {131072+R,35,B};
decode_f(<<2#1111111111111111110:19,R:18,B/bits>>) -> {262144+R,37,B};
decode_f(<<2#11111111111111111110:20,R:19,B/bits>>) -> {524288+R,39,B};
decode_f(<<2#111111111111111111110:21,R:20,B/bits>>) -> {1048576+R,41,B};
decode_f(<<2#1111111111111111111110:22,R:21,B/bits>>) -> {2097152+R,43,B};
decode_f(<<2#11111111111111111111110:23,R:22,B/bits>>) -> {4194304+R,43,B};
decode_f(Bin) ->
    io:format("Cannot decode:~p~n",[Bin]),
    0.

%%----------------------------------------------------------------------

%% encode_seq([Int]) -> bits()

-spec encode_seq([integer()]) -> bitstring().

%% encode a sequence of positive integers

encode_seq(L) ->
    encode_seq(L, <<>>).

encode_seq([H|T], B) ->
    BH = encode(H),
    encode_seq(T, <<BH/bits,B/bits>>);
encode_seq([], B) ->
    B.

-spec decode_seq(bitstring()) -> [integer()].

%% decode_seq(bits()) -> [Int]

decode_seq(B) when is_bitstring(B) ->
    decode_seq(B, bit_size(B), []).

decode_seq(_, 0, L) ->
    L;
decode_seq(B, N, L) ->
    {Int,Size, B1} = decode(B),
    decode_seq(B1, N-Size, [Int|L]).

pow(N, 1) -> N;
pow(N, M) -> N * pow(N, M-1).

%%----------------------------------------------------------------------
%% Compression and decompression of
%%   ascending lists with no duplicates
%%   These are inverses
%%   alist_to_gamma(L)   -> Bin
%%   gamma_to_alist(Bin) -> L.

%% actually this is an opaque type
-spec alist_to_gamma([integer()]) -> binary().

alist_to_gamma(L) ->
    Bin = term_to_binary(alist_to_gamma1(L)),
    case gamma_to_alist(Bin) of
	L -> Bin;
	_ -> exit({eAlistToGamma,L})
    end.

alist_to_gamma1([L]) ->
    {one, L};
alist_to_gamma1([H|T]) ->
    L  = dlist(H, T),
    B  = encode_seq(L),
    {dlist,H,B}.

-spec dlist([integer()]) -> {dlist, Start::integer(), Diffs::[integer()]}.

%% turns a sorted list of different integers into a structure
%% containing the first element of the list and a list of deltas

dlist([H|T]) ->
    {dlist, H, dlist(H, T)}.

dlist(H1, [H2|T]) -> [H2-H1|dlist(H2, T)];
dlist(_H, [])     -> [].

gamma_to_alist(Bin) ->
    case binary_to_term(Bin) of
	{one, X} ->
	    [X];
	{dlist, First, Bin1} ->
	    L1 = decode_seq(Bin1),
	    [First|make_list(First, L1)]
    end.

make_list(X, [H|T]) -> [X+H|make_list(X+H, T)];
make_list(_, [])    -> [].






