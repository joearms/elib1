%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(test).
-compile(export_all).
-import(lists, [map/2, reverse/1]).

%% -------------- begin ----------------
%% DEFN s = " "* ;

%% Abstract syntax:
%%   {star,{lit," "}}
%% 

parse_s(S)->
    {Val, S1} =
        (epeg_pc:star(fun(" " ++ V1) -> {' ', V1} end ))(S),
    {epeg_pc:done(?MODULE,s,Val), S1}.

%% =============== end =================

%% -------------- begin ----------------
%% DEFN line = s (token s)* ;

%% Abstract syntax:
%%   {seq,[{nt,"s"},{star,{seq,[{nt,"token"},{nt,"s"}]}}]}
%% 

parse_line(S)->
    {Val, S1} =
        (epeg_pc:seq([
              fun parse_s/1,
              epeg_pc:star(epeg_pc:seq([
                  fun parse_token/1,
                  fun parse_s/1]))]))(S),
    {epeg_pc:done(?MODULE,line,Val), S1}.

%% =============== end =================

%% -------------- begin ----------------
%% DEFN token = peglib:float | peglib:int | atom ;

%% Abstract syntax:
%%   {alt,[{nt,"peglib","float"},
%%         {nt,"peglib","int"},
%%         {nt,"atom"}]}
%% 

parse_token(S)->
    {Val, S1} =
        (epeg_pc:alt([
              fun peglib:parse_float/1,
              fun peglib:parse_int/1,
              fun parse_atom/1]))(S),
    {epeg_pc:done(?MODULE,token,Val), S1}.

%% =============== end =================

%% -------------- begin ----------------
%% DEFN atom = [a-z]+ ;

%% Abstract syntax:
%%   {plus,{class,[{97,122}]}}
%% 

parse_atom(S)->
    {Val, S1} =
        (epeg_pc:plus(epeg_pc:cc([{97,122}])))(S),
    {epeg_pc:done(?MODULE,atom,Val), S1}.

%% =============== end =================



test() ->
    {Toks, []} = test:parse_line("    1234 abc 12.23 34"),
    [{token,1234},
     {token,{atom,"abc"}},
     {token,{float,_}},
     {token,34}] = Toks,
    io:format("test:test worked~n"),
    init:stop().

final(line, [_,L]) -> 
    [I || [I,_] <- L];
final(Tag, Val) -> 
    {Tag, Val}.


