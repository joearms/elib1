%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(elib1_rsa).
%% Author: Joe Armstrong

%% This is an "out of the book" implementation of RSA
%% Note: RSA is generally reckoned to be about 1500 times slower than DES
%% for a 512 but key. Encryption rates of 600 KBits/sec have beeen achieved
%% on special purpose hardware.
%% Our implementation achieves about 77 bits/sec - not blindingly fast
%% (but the code is neat!)

%% Now we need to look at RFC??? to find the fomat of public key
%% certificates
-compile(export_all).

-export([test1/0, test2/0, test3/0, sign_bin/2,
	 make_sig/1, make_sig/2, get_key/1, decode_sig/2,
	 encode/3, decode/3]).

-import(lists, [reverse/1]).

test1() ->
    P = 101,
    Q = 113,
    N = P*Q,
    Phi = (P-1)*(Q-1),
    B = 3533,
    A = elib1_lin:inv(B, Phi),
    Secret = 9726,
    Encoded = elib1_lin:pow(Secret, B, N),
    Decoded = elib1_lin:pow(Encoded, A, N),
    case Secret of
	Decoded -> ok;
	_ -> error
    end.

test2() ->
    test2("Hello world").

sign_bin(Bin, PubKey) ->
    Str = binary_to_list(Bin),
    N = elib1_lin:str2int(Str),
    code(N, PubKey).

decode_sig(Int, Code) ->
    N = code(Int, Code),
    Str = elib1_lin:int2str(N),
    list_to_binary(Str).

test2(Str) ->
    {Private, Public} = make_sig_len(50),
    I = elib1_lin:str2int(Str),
    Coded = code(I, Public),
    J = code(Coded, Private),
    Str = elib1_lin:int2str(J),
    true.

test3() ->
    encode("bug.erl", "joe.pub", "bug.x"),
    decode("bug.x",   "joe.pri", "bug.y").
    
test4() ->
    Str = "12345",
    Md5 = erlang:md5(Str),
    S1 = binary_to_list(Md5),
    Int = codeWithKeyFile(S1, "joe.pri"),
    Str2 = elib1_lin:int2list(Int,62),
    Proof = [enc(I) || I <- Str2],
    %% now start decoding
    Str2 = [dec(I) ||  I <- Proof],
    Int1 = elib1_lin:list2int(Str2, 62),
    Int2 = codeWithKeyFile(Int1, "joe.pub"),
    S1 = elib1_lin:int2str(Int2).
		
get_key(File) ->
    {ok, B} = file:read_file(File),
    binary_to_term(B).
       
test5() ->
    {Priv,_Pub} = make_sig_len(77),
    io:format("~p~n",[Priv]),
    Str = "12345",
    Md5 = erlang:md5(Str),
    S1 = binary_to_list(Md5),
    I1 = elib1_lin:str2int(S1),
    io:format("Md5=~p~n",[I1]),
    Sig = code(I1, Priv),
    _Short = int2short(Sig).

int2short(BigInt) ->
    Is = elib1_lin:int2list(BigInt, 62),
    [enc(I) || I <- Is].

enc(X) when 0 =< X, X =< 9 -> X+$0;
enc(X) when 10 =< X, X =< 35 -> X+$a-10;
enc(X) when 36 =< X, X =< 61 -> X+$A-36.    

dec(X) when $0 =< X, X =< $9 -> X-$0;
dec(X) when $a =< X, X =< $z -> X-$a+10;
dec(X) when $A =< X, X =< $Z -> X-$A+36.    

codeWithKeyFile(Str, KeyFile) ->
    Int = elib1_lin:str2int(Str),
    {ok, Bin} = file:read_file(KeyFile),
    Key = binary_to_term(Bin),
    code(Int, Key).

code(X, {A,N}) -> elib1_lin:pow(X, A, N).

%% A few notes here
%% RSA needs large (for example 80 digit) primes
%% Typically a 512 bit signed integer is used
%% 2^256 = 1.15e+77 (math:pow(2,256)
%% which gives a 77 digit integer
%% generating a 512 bit random prime needs
%% approximately 177 trials (math:log(math:pow(2, 256)))
%% 80 digits requires on average 184 trials (math:log(math:pow(10,80)))
%% i.e the average "gap" between (odd) primes is 184 for 80
%% digit primes

make_sig(Who) ->
    make_sig(Who, 100).

make_sig(Who, Len) when Len > 79 ->
    {Public, Private} = make_sig_len(Len),
    file:write_file(Who ++ ".pub", term_to_binary(Public)),
    file:write_file(Who ++ ".pri", term_to_binary(Private)),
    {keyfilecreated,for,Who}.

%% The "book" says ...
%% 1. Bob generates two primes p and q
%% 2. Bob computes n = pq and phi(n) = (p-1)*(q-1)
%% 3. Bob chooses a random b(0 < b < phi(n)) such that
%%    gcd(b, phi(n)) = 1
%% 4. Bob computes a = b^(-1) mod phi(n) using the Euclidean algorithm
%% 5. Bob publishes n and  b in a directory as his public key.

make_sig_len(Len) ->
    %% generate two <Len> digit prime numbers
    P = elib1_ran:make_prime(Len),
    io:format("P = ~p~n", [P]),
    Q = elib1_ran:make_prime(Len),
    io:format("Q = ~p~n", [Q]),
    N = P*Q,
    io:format("N = ~p~n", [N]),
    Phi = (P-1)*(Q-1),
    %% now make B such that B < Phi and gcd(B, Phi) = 1
    B = b(Phi),
    io:format("Public key (B) = ~p~n", [B]),
    A = elib1_lin:inv(B, Phi),
    io:format("Private key (A) = ~p~n", [A]),
    {{B,N},{A,N}}.

b(Phi) ->
    io:format("Generating a public key B "),
    K = length(integer_to_list(Phi)) - 1,
    B = b(1, K, Phi),
    io:format("~n", []),
    B.

b(Try, K, Phi) ->
    io:format("."),
    B = elib1_ran:make(K),
    if 
	B < Phi ->
	    case elib1_lin:gcd(B, Phi) of
		1 -> B;
		_ -> b(Try+1, K, Phi)
	    end;
	true ->
	    b(Try, K, Phi)
    end.

encode(In, KeyFile, Out) ->
    case file:read_file(In) of
	{ok, Bin} ->
	    case file:read_file(KeyFile) of
		{ok, Bin1} ->
		    Key = binary_to_term(Bin1),
		    NBytes = size(Bin),
		    statistics(runtime),
		    E = enc(binary_to_list(Bin), Key, []),
		    {_,Time} = statistics(runtime),
		    Rate = NBytes/(Time/1000),
		    io:format("~p bytes encryped in ~p ms.~n",[NBytes,Time]),
		    io:format("Encryption rate = ~p bytes/sec~n",[Rate]),
		    file:write_file(Out, term_to_binary(E));
		_ ->
		    error_reading_keyfile
	    end;
	_ ->
	    error_reading_datafile
    end.

enc([B1,B2,B3,B4,B5,B6,B7,B8,B9,B10|T], Key, L) ->
    Str = [B1,B2,B3,B4,B5,B6,B7,B8,B9,B10],
    io:format(".",[]),
    Int = elib1_lin:str2int(Str),
    enc(T, Key, [code(Int, Key)|L]);
enc(Str, Key, L) ->
    io:format(".~n",[]),
    Int = elib1_lin:str2int(Str),
    [code(Int, Key)|L].

decode(In, KeyFile, Out) ->
    case file:read_file(In) of
	{ok, Bin} ->
	    case file:read_file(KeyFile) of
		{ok, Bin1} ->
		    Key = binary_to_term(Bin1),
		    statistics(runtime),
		    Str = dec(binary_to_term(Bin), Key, []),		    
		    NBytes = length(Str),
		    {_,Time} = statistics(runtime),
		    Rate = NBytes/(Time/1000),
		    io:format("~p bytes decryped in ~p ms.~n",[NBytes,Time]),
		    io:format("Decryption rate = ~p bytes/sec~n",[Rate]),
		    file:write_file(Out, list_to_binary(Str));
		_ ->
		    error_reading_keyfile
	    end;
	_ ->
	    error_reading_datafile
    end.

dec([H|T], Key, L) ->
    Decoded = code(H, Key),
    Str = elib1_lin:int2str(Decoded),
    io:format(".",[]),
    dec(T, Key, Str ++ L);
dec([], _Key, L) ->
    io:format("~n",[]),
    L.



