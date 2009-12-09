%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

%% ^ [4]

-module(elib1_best). %% [1]

%% elib1_best: Best practice template for library modules [2]
%% Time-stamp: <2009-12-02 12:28:04 ejoearm> [3]

-include_lib("eunit/include/eunit.hrl"). %% [5]

-export([fac/1]).  %% [6]


%% @doc fac(N) computes factorial(N) using a fast iterative algorithm. [7]

-spec fac(non_neg_integer()) -> non_neg_integer(). %% [8]

fac(N) when is_integer(N), N >= 0 -> fac1(N, 1).

fac1(0, K) -> K;
fac1(N, K) -> fac1(N-1, K*N).

fac_test() ->  %% [9]
    6  = fac(3),
    24 = fac(4).

%% Notes:
%% [1] - module on line 1
%% [2] - module comment
%% [3] - Time stamp auto genertaed by emacs. Must be near start of file
%% [4] - Copyright (I always forget this, but adding a ciopyright reduces
%%       the pain later
%% [5] - Needed for eunit
%% [6] - use export and NOT compile(export_all)
%% [7] - @doc comes first
%% [8] - -spec comes immediately *before* the function
%% [9] - test cases come immediatly after the function

%% end of module

    
    
