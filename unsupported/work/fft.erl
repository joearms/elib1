%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(fft).
-compile(export_all).

%% e(ix) = cos x + i sin x
%% DFT(K) = sum(n=0,n=N-1)x[N] e(-2*pi*i)*n*K/N
%% X=x[0]..x[N-1] (N=size of vector)

-import(lists, [zip/2, unzip/1, reverse/1, seq/2, sum/1]).

%% e(ix) = cos(x) + i*sin(x) for real X
%%         cos(-2*Pi*K*N/Len) + i * sin(....)

-define(PI, 3.1415926535).

timer() ->
    L = [ran_complex(I) || I <- seq(1,1024)],
    {T1, L1} = timer:tc(?MODULE, fft, [L]),
    {T2, L2} = timer:tc(?MODULE, dft, [L]),
    Frames = 44000/1024,
    Tot = Frames * T1 / 1000000,
    {{test,is,equals(L1, L2)}, {timeFor1secAnalysis,Tot,sec}, 
     {fft,for,1024,points,took,T1,us}, {dft,took,T2}, 
     {speedup,T2/T1}}.


ran_complex(_) ->
    {random:uniform(), random:uniform()}.

test() ->
    L = [element(1, test(I)) || I <- [1,2,3,4,5]],
    L1 = [test1() | L],
    L2 = test_fft(),
    L1 ++ L2.

tv(1) ->
    [{1,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0}];
tv(2) ->
    [{0,0},{1,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0}];
tv(3) ->
    [{1,0},{0,1},{-1,0},{0,-1}, {1,0},{0,1},{-1,0},{0,-1}];
tv(4) ->
    [{1,2},{2,3},{4,5},{3,2},{1,6},{2,4},{2,1},{4,5}];
tv(5) ->
    [{1,2},{7,7},{2,3},{6,8}].

test(I) -> test_dft(tv(I)).

test_fft() ->
    [test_fft(I) || I <- seq(1,5)].

test_fft(N) ->
    equals(dft(tv(N)), fft(tv(N))).


test1() ->
    %% http://lib.stat.cmu.edu/sapaclisp/dft-and-fft.lisp
    Kay = tv(3),
    Result = [{0,0},{0,0},{8,0},{0,0},{0,0},{0,0},{0,0},{0,0}],
    equals(dft(Kay), Result).

test_dft(X) ->
    D = dft(X),
    I = inv_dft(D),
    io:format("X=~p~nD=~p~nI=~p~n",[X,D,I]),
    E = equals(X, I),
    {E, X, D, I}.

equals(L1, L2) ->
    L = lists:zip(L1, L2), 
    %% io:format("L=~p~n",[L]),
    lists:all(fun near/1, L).

near({{R1,I1},{R2,I2}}) ->
    X1 = (R1 - R2), 
    X2 = (I1 - I2),
    X1*X1 + X2*X2 < 1.0e-6.

%% FFT using Cooley-Turkey Algorithm
fft([_,_]=X) -> dft(X);
fft(L) ->
    {Evens, Odds} = split(L, [], []),
    LE = fft(Evens),
    LO = fft(Odds),
    Len = length(L),
    LF = mult(LO, 0, -2*?PI/Len),
    H1 = lists:zipwith(fun(E, F) -> cadd(E, F) end, LE, LF),
    H2 = lists:zipwith(fun(E, F) -> csub(E, F) end, LE, LF),
    H1 ++ H2.

cadd({R1,I1},{R2,I2}) -> {R1+R2,I1+I2}.

csub({R1,I1},{R2,I2}) -> {R1-R2,I1-I2}.

mult([], _, _)    -> [];
mult([O|T], K, M) -> [cmult(O, e(K*M))|mult(T, K+1, M)].


split([H1,H2|T], E, O) -> split(T, [H1|E], [H2|O]);
split([], E, O)        -> {reverse(E), reverse(O)}.
    



%% Discrete Fourier Transform of a tuple of N complex numbers
%% = Tuple of N complex numbers


dft(X) ->
    %% Exponent = -2*pi*i*K*n/Len
    %% e(iX) = cos(X) + i sin(X)
    Len = length(X),
    Const = -2*?PI/Len,
    [bigX(K, X, Const) || K <- seq(0, Len-1)].

inv_dft(X) ->
    %% Exponent = -2*pi*i*K*n/Len
    %% e(iX) = cos(X) + i sin(X)
    Len = length(X),
    Const = 2*?PI/Len,
    lists:map(fun(K) ->
		      {R,I} = bigX(K, X, Const),
		      {R/Len, I/Len}
	end, seq(0, Len-1)).

bigX(K, X, Const) ->
    sum(X, 0, Const*K, 0, 0).

sum([], _, _, R, I) -> {R,I};
sum([Xn|T], N, Const, R1, I1) ->
    X = Const * N,
    C1 = e(X),
    {R2, I2} = cmult(Xn, C1),
    sum(T, N+1, Const, R1 + R2, I1 + I2).

e(X) ->
    {math:cos(X), math:sin(X)}.

cmult({R1,I1},{R2,I2}) ->
    {R1*R2 - I1*I2,R1*I2+R2*I1}.
