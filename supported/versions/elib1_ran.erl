%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(elib1_ran).
%% Author: Joe Armstrong

-compile(export_all).

-export([make/1, make_prime/1, is_prime/1]).

%% make a prime of N decimal digits
make_prime(N) ->
    ensure_seeded(),
    io:format("Generating a ~p digit prime ",[N]),
    D = make(N),
    D1 = make_prime(1, D, N),
    io:format("~n",[]),
    D1.

make_prime(1000,_,_) ->
    no;
make_prime(K, D, N) ->
    io:format(".",[]),
    case is_prime(100, D, N) of
	yes -> D;
	{no,X,Y} ->
	    %% io:format("   ~p ~p~n",[X,Y]),
	    make_prime(K+1,D+1,N)
    end.


%% make(N) -> a random integer with N digits.

make(N) -> ensure_seeded(), make(N, 0).

make(0, D) -> D;
make(N, D) ->
    make(N-1, D*10 + (random:uniform(10)-1)).

is_prime(D) ->
    ensure_seeded(),
    %% figure out the number of digits in D
    N = length(integer_to_list(D)) -1,
    is_prime(100, D, N).

is_prime(0, _, _) -> yes;
is_prime(Ntest, D, N) ->
    %% generate a random number A less than D
    K = random:uniform(N),
    A = make(K),
    if
	A < D ->
	    %% fermat's theormem says that if D is a prime and if A is
	    %% less than A then
	    %% A^D mod N = A
	    case lin:pow(A,D,D) of
		A -> is_prime(Ntest-1,D,N);
		_ -> {no,A,D}
	    end;
	true ->
	    is_prime(Ntest, D, N)
    end.

ensure_seeded() ->
    case get(random_seed) of
	undefined ->
	    random:seed();
	_ ->
	    true
    end.
