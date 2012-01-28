-module(elib1_parse_dtd).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("elib1_parse_dtd.yrl", 116).

%% -compile(export_all).

-export([test/1, parse_dtd_in_string/2, parse_dtd_in_file/2]).

-import(lists, [filter/2, member/2, partition/2, reverse/1]).

-define(IN(X,L,H), L=<X,X=<H).

fix_type(A, nil) -> A;
fix_type(A, {'$seq',B}) -> {'$seq',[A|B]};
fix_type(A, {'$alt',B}) -> {'$alt', [A|B]}.

fix_mod(none, X) -> X;
fix_mod(Mod, X) -> {Mod, X}.

unwrap2({_,X,_}) -> X.

unwrap3({_,_,X}) -> X.


fix_name({_,_,X}) ->
    list_to_atom(X).

cata(_Pub, Local) ->
    case file:read_file(Local) of
	{ok, Bin} ->
	    {ok, binary_to_list(Bin)};
	_ ->
	    error
    end.

test(1) -> parse_dtd_in_file("chap.dtd", fun cata/2);
test(2) -> parse_dtd_in_file("log.dtd", fun cata/2);
test(3) -> parse_dtd_in_file("xhtml1-transitional.dtd", fun cata/2).

parse_dtd_in_file(File, Catalogue) ->
    io:format("Parsing:~s~n",[File]),
    Str = file2string(File),
    parse_dtd_in_string(Str, Catalogue).

parse_dtd_in_string(Str, Catalogue) ->
    Toks = tokenize(Str, 1, []),
    elib1_misc:dump("toks", Toks),
    {Forms, _Ents} = collect_stuff(Toks, [], [], Catalogue),
    elib1_misc:dump("forms", Forms),
    Parsed = [do_parse(I) || I <- Forms],
    elib1_misc:dump("parsed0", Parsed),
    check_grammar(Parsed),
    Parsed1 = merge_attribues_and_elements(Parsed),
    elib1_misc:dump("parsed", Parsed1),
    Parsed1.

file2string(File) ->
    {ok, Bin} = file:read_file(File),
    binary_to_list(Bin).

collect_stuff([{'%',_},{name,Ln,Name},{';',_}|T], Forms, Ents, F) ->
    %% expand the entity
    T1 = expand_entity(Name, Ln, Ents),
    collect_stuff(T1 ++ T, Forms, Ents, F);
collect_stuff([{'<!', _},{entity,_},{'%',_},{name,_,Name},
	       {string,_,Str},{'>',_}|Toks],
	      Forms, Ents, F) ->
    %% io:format("Name=~p Str=~p~n",[Name,Str]),
    Str1 = expand_entities_in_string(Str, Name, 1, Ents),
    collect_stuff(Toks, Forms, [{Name,Str1}|Ents], F);
collect_stuff([{'<!', _},{entity,_},{'%',_},{name,_,Name},{name,_,_Vis},
	       {string,_,StdName},{string,_,LocalName},{'>',_}|Toks],
	      Forms, Ents, F) ->
    %% Vis = "PUBLIC" | "PRIVATE"
    %% Defining a parameter entity
    %% fetch the entity and store as a string
    Ents1 = case F(StdName, LocalName) of
		{ok, Str} ->
		    [{Name,Str}|Ents];
		error ->
		    io:format("*** cannot locate entity:~p ~p~n",
			      [StdName, LocalName]),
		    Ents
	    end,
    collect_stuff(Toks, Forms, Ents1, F);
collect_stuff([{'<!', _}|T], Forms, Ents, F) ->
    {InnerToks, T1} = collect(T, []),
    %% now expand any inner entities
    InnerToks1 = expand_entities(InnerToks, Ents),
    %% io:format("Form=~p~n",[InnerToks1]),
    collect_stuff(T1, [{form,InnerToks1}|Forms], Ents, F);
collect_stuff([], Forms, Ents, _) ->
    {reverse(Forms), reverse(Ents)};
collect_stuff(T, _Forms, _Ents, _F) ->
    io:format("cannot understand:~p~n",[string:sub_string(T,1,10)]),
    exit(oops).

collect([{'>',_}|Toks], L) -> {reverse(L), Toks};
collect([H|T], L)          -> collect(T, [H|L]);
collect([], _)             -> exit(eof_in_form).

do_parse({form, [{entity,_},{name,_,N},{string,_,S}]}) ->
    {entity, {N,S}};
do_parse({form, Toks}) ->
    case parse(Toks) of
	{error,{Line,Mod,Args}} ->
	    io:format("Parse:~p~n",[Toks]),
	    Str = lists:flatten(Mod:format_error(Args)),
	    io:format("Error line:~w ~s~n",[Line, Str]),
	    exit(error);
	{ok, P} ->
	    P
    end.

expand_entities_in_string(_, Name, 20, _) ->
    exit({tooDeepRecursionInExpandEntity,Name});
expand_entities_in_string([$%|T], Name, Level, E) ->
    case collect_entity_name(T, []) of
	{EntName, T1} ->
	    case elib1_misc:lookup(EntName, E) of
		{ok, Str} ->
		    expand_entities_in_string(Str ++ T1, Name, Level+1, E);
		error ->
		    exit({undefinedEntity,EntName})
	    end;
	no ->
	    [$%|expand_entities_in_string(T, Name, Level, E)]
     end;
expand_entities_in_string([H|T], Name, Level, E) ->
    [H|expand_entities_in_string(T, Name, Level, E)];
expand_entities_in_string([], _, _, _) ->
    [].

collect_entity_name([$;|T], L) -> {reverse(L), T};
collect_entity_name([H|T], L)  -> collect_entity_name(T, [H|L]);
collect_entity_name([], _)     -> error.


expand_entities([{'%',_},{name,Ln1,Name},{';',_}|T], Ents) ->
    expand_entity(Name, Ln1, Ents) ++ expand_entities(T, Ents);
expand_entities([H|T], Ents) ->
    [H|expand_entities(T, Ents)];
expand_entities([], _) ->
    [].

expand_entity(Name, Ln, Ents) ->
    case elib1_misc:lookup(Name, Ents) of
	{ok, Str} -> tokenize(Str, Ln, []);
	error     -> exit({undefined,entity,Name})
    end.

skip_comment("-->" ++ T, Ln) -> {Ln, T};
skip_comment([$\n|T], Ln)    -> skip_comment(T, Ln+1);
skip_comment([_|T], Ln)      -> skip_comment(T, Ln).

tokenize("<!--" ++ T, Ln, L) ->
    {Ln1, T1} = skip_comment(T, Ln),
    tokenize(T1, Ln1, L);
tokenize("<!" ++ T, Ln, L) ->
    tokenize(T, Ln, {'<!',Ln}, L);

tokenize("ATTLIST" ++ T, Ln, L) ->
    tokenize(T, Ln, {attlist,Ln}, L);
tokenize("EMPTY" ++ T, Ln, L) ->
    tokenize(T, Ln, {empty,Ln}, L);
tokenize("CDATA" ++ T, Ln, L) ->
    tokenize(T, Ln, {attype,cdata,Ln}, L);
tokenize("IDREFS" ++ T, Ln, L) ->
    tokenize(T, Ln, {attype,idRefs,Ln}, L);
tokenize("IDREF" ++ T, Ln, L) ->
    tokenize(T, Ln, {attype,idRef,Ln}, L);
tokenize("ID" ++ T, Ln, L) ->
    tokenize(T, Ln, {attype,id,Ln}, L);
tokenize("ENTITIES" ++ T, Ln, L) ->
    tokenize(T, Ln, {attype,entities,Ln}, L);
tokenize("NMTOKENS" ++ T, Ln, L) ->
    tokenize(T, Ln, {attype,nmtokens,Ln}, L);
tokenize("NMTOKEN" ++ T, Ln, L) ->
    tokenize(T, Ln, {attype,nmtoken,Ln}, L);
tokenize("#PCDATA" ++ T, Ln, L) ->
    tokenize(T, Ln, {pcdata,Ln}, L);
tokenize("#REQUIRED" ++ T, Ln, L) ->
    tokenize(T, Ln, {required,Ln}, L);
tokenize("#IMPLIED" ++ T, Ln, L) ->
    tokenize(T, Ln, {implied,Ln}, L);
tokenize("#FIXED" ++ T, Ln, L) ->
    tokenize(T, Ln, {fixed,Ln}, L);
tokenize("ELEMENT" ++ T, Ln, L) ->
    tokenize(T, Ln, {element,Ln}, L);
tokenize("ENTITY" ++ T, Ln, L) ->
    tokenize(T, Ln, {entity,Ln}, L);
tokenize([H|T], Ln, L) when H =:= $(; H =:= $);
                            H =:= $=;
			    H =:= $%; H =:= $;;
			    H =:= $|; H == $*;
			    H =:= $/;
                            H =:= $$; H == $[;
                            H == $#;
			    H =:= $,; H =:= $>;
                            H =:= $+; H =:= $?; H =:= $] ->
    tokenize(T, Ln, {list_to_atom([H]),Ln}, L);
tokenize([$\n|T], Ln, L) ->
    tokenize(T, Ln+1, L);
tokenize([$"|T], Ln, L) ->
    {Str, Ln1, T1} = collect_string(T, $", Ln, []),
    tokenize(T1, Ln1, {string,Ln,Str}, L);
tokenize([$'|T], Ln, L) ->
    {Str, Ln1, T1} = collect_string(T, $', Ln, []),
    tokenize(T1, Ln1, {string,Ln,Str}, L);
tokenize([H|T], Ln, L) when H =:= $\s; H =:= $\t;
                            H =:= $\r ->
    tokenize(T, Ln, L);
tokenize([H|T], Ln, L) when ?IN(H, $a, $z);?IN(H,$A,$Z);?IN(H,$0,$9) ->
    {Name, T1} = collect_name(T, [H]),
    tokenize(T1, Ln, {name,Ln,Name}, L);
tokenize([H|T], Ln, L) ->
    io:format("** invalid character:~p~n",[H]),
    tokenize(T, Ln, L);
tokenize([], _, L) ->
    reverse(L).

tokenize(Str, Ln, Tok, L) ->
    %% io:format("~w:~p~n",[Ln, Tok]),
    tokenize(Str, Ln, [Tok|L]).

collect_name([H|T], L) when ?IN(H, $a, $z);?IN(H,$A,$Z);?IN(H,$0,$9) ->
    collect_name(T, [H|L]);
collect_name([H|T], L) when H =:= $.; H=:= $-; H =:= $:->
    collect_name(T, [H|L]);
collect_name(T, L) ->
    {reverse(L), T}.

collect_string([$\n|T], Stop, Ln, L) -> collect_string(T, Stop, Ln+1, [$\n|L]);
collect_string([H|T], H, Ln, L)      -> {reverse(L), Ln, T};
collect_string([], _Stop, Ln, L)     -> throw({eof_in_string,Ln,L});
collect_string([H|T], Stop, Ln, L)   -> collect_string(T, Stop, Ln, [H|L]).

%%----------------------------------------------------------------------
%% Some checking

check_grammar(G) ->
    Rhs = [R || {{element,_},R} <- G],
    Used = atoms_in(Rhs, []),
    io:format("Used=~p~n",[Used]),
    Defined = [X || {{element,X},_} <- G],
    case elib1_misc:duplicates(Defined) of
	[] -> void;
	L  -> exit({eDuplicatedSymbols, L})
    end,
    Missing = filter(fun(I) -> not member(I, Defined) end, Used),
    Missing1 = Missing -- ['$question','$plus','$empty','$mixed',
                           '$seq','$star','$alt','$pcdata'],
    case Missing1 of
	[] -> void;
	_  -> exit({eUndefinedSymbols, Missing1})
    end,
    Attrs = [X || {{attlist,X},_} <- G],
    Missing2 = filter(fun(I) -> not member(I, Defined) end, Attrs),
    case Missing2 of
	[] -> void;
	_ -> exit({attributeHasNoElement,Missing2})
    end.


atoms_in(H, L) when is_atom(H) ->
    case member(H, L) of
	true  -> L;
	false -> [H|L]
    end;
atoms_in(T, L) when is_tuple(T) ->
    atoms_in(tuple_to_list(T), L);
atoms_in([H|T], L) ->
    atoms_in(H, atoms_in(T, L));
atoms_in(_, L) ->
    L.

merge_attribues_and_elements(G) ->
    {Elems, Attrs} = lists:partition(fun({{element,_I},_}) -> true;
					(_) -> false
				     end, G),
    [{I, {Def, get_attr(I, Attrs)}} || {{element,I},Def} <- Elems].

get_attr(I, Attrs) ->
    elib1_misc:lookup({attlist,I}, Attrs).

-file("/usr/local/lib/erlang/lib/parsetools-2.0.1/include/yeccpre.hrl", 0).
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

-type(yecc_ret() :: {'error', _} | {'ok', _}).

-spec parse(Tokens :: list()) -> yecc_ret().
parse(Tokens) ->
    yeccpars0(Tokens, {no_func, no_line}, 0, [], []).

-spec(parse_and_scan/1 ::
      ({function() | {atom(), atom()}, [_]} | {atom(), atom(), [_]}) ->
            yecc_ret()).
parse_and_scan({F, A}) -> % Fun or {M, F}
    yeccpars0([], {{F, A}, no_line}, 0, [], []);
parse_and_scan({M, F, A}) ->
    yeccpars0([], {{{M, F}, A}, no_line}, 0, [], []).

-spec(format_error/1 :: (any()) -> [char() | list()]).
format_error(Message) ->
    case io_lib:deep_char_list(Message) of
	true ->
	    Message;
	_ ->
	    io_lib:write(Message)
    end.

% To be used in grammar files to throw an error message to the parser
% toplevel. Doesn't have to be exported!
-compile({nowarn_unused_function, return_error/2}).
-spec(return_error/2 :: (integer(), any()) -> no_return()).
return_error(Line, Message) ->
    throw({error, {Line, ?MODULE, Message}}).

-define(CODE_VERSION, "1.4").

yeccpars0(Tokens, Tzr, State, States, Vstack) ->
    try yeccpars1(Tokens, Tzr, State, States, Vstack)
    catch
        error: Error ->
            Stacktrace = erlang:get_stacktrace(),
            try yecc_error_type(Error, Stacktrace) of
                {syntax_error, Token} ->
                    yeccerror(Token);
                {missing_in_goto_table=Tag, Symbol, State} ->
                    Desc = {Symbol, State, Tag},
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                 Stacktrace)
            catch _:_ -> erlang:raise(error, Error, Stacktrace)
            end;
        %% Probably thrown from return_error/2:
        throw: {error, {_Line, ?MODULE, _M}} = Error ->
            Error
    end.

yecc_error_type(function_clause, [{?MODULE,F,[State,_,_,_,Token,_,_]} | _]) ->
    case atom_to_list(F) of
        "yeccpars2" ++ _ ->
            {syntax_error, Token};
        "yeccgoto_" ++ SymbolL ->
            {ok,[{atom,_,Symbol}],_} = erl_scan:string(SymbolL),
            {missing_in_goto_table, Symbol, State}
    end.

yeccpars1([Token | Tokens], Tzr, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens, Tzr);
yeccpars1([], {{F, A},_Line}, State, States, Vstack) ->
    case apply(F, A) of
        {ok, Tokens, Endline} ->
	    yeccpars1(Tokens, {{F, A}, Endline}, State, States, Vstack);
        {eof, Endline} ->
            yeccpars1([], {no_func, Endline}, State, States, Vstack);
        {error, Descriptor, _Endline} ->
            {error, Descriptor}
    end;
yeccpars1([], {no_func, no_line}, State, States, Vstack) ->
    Line = 999999,
    yeccpars2(State, '$end', States, Vstack, yecc_end(Line), [],
              {no_func, Line});
yeccpars1([], {no_func, Endline}, State, States, Vstack) ->
    yeccpars2(State, '$end', States, Vstack, yecc_end(Endline), [],
              {no_func, Endline}).

%% yeccpars1/7 is called from generated code.
%%
%% When using the {includefile, Includefile} option, make sure that
%% yeccpars1/7 can be found by parsing the file without following
%% include directives. yecc will otherwise assume that an old
%% yeccpre.hrl is included (one which defines yeccpars1/5).
yeccpars1(State1, State, States, Vstack, Token0, [Token | Tokens], Tzr) ->
    yeccpars2(State, element(1, Token), [State1 | States],
              [Token0 | Vstack], Token, Tokens, Tzr);
yeccpars1(State1, State, States, Vstack, Token0, [], {{_F,_A}, _Line}=Tzr) ->
    yeccpars1([], Tzr, State, [State1 | States], [Token0 | Vstack]);
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, no_line}) ->
    Line = yecctoken_end_location(Token0),
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line});
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, Line}) ->
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line}).

% For internal use only.
yecc_end({Line,_Column}) ->
    {'$end', Line};
yecc_end(Line) ->
    {'$end', Line}.

yecctoken_end_location(Token) ->
    try
        {text, Str} = erl_scan:token_info(Token, text),
        {line, Line} = erl_scan:token_info(Token, line),
        Parts = re:split(Str, "\n"),
        Dline = length(Parts) - 1,
        Yline = Line + Dline,
        case erl_scan:token_info(Token, column) of
            {column, Column} ->
                Col = byte_size(lists:last(Parts)),
                {Yline, Col + if Dline =:= 0 -> Column; true -> 1 end};
            undefined ->
                Yline
        end
    catch _:_ ->
        yecctoken_location(Token)
    end.

yeccerror(Token) ->
    Text = yecctoken_to_string(Token),
    Location = yecctoken_location(Token),
    {error, {Location, ?MODULE, ["syntax error before: ", Text]}}.

yecctoken_to_string(Token) ->
    case catch erl_scan:token_info(Token, text) of
        {text, Txt} -> Txt;
        _ -> yecctoken2string(Token)
    end.

yecctoken_location(Token) ->
    case catch erl_scan:token_info(Token, location) of
        {location, Loc} -> Loc;
        _ -> element(2, Token)
    end.

yecctoken2string({atom, _, A}) -> io_lib:write(A);
yecctoken2string({integer,_,N}) -> io_lib:write(N);
yecctoken2string({float,_,F}) -> io_lib:write(F);
yecctoken2string({char,_,C}) -> io_lib:write_char(C);
yecctoken2string({var,_,V}) -> io_lib:format("~s", [V]);
yecctoken2string({string,_,S}) -> io_lib:write_unicode_string(S);
yecctoken2string({reserved_symbol, _, A}) -> io_lib:write(A);
yecctoken2string({_Cat, _, Val}) -> io_lib:write(Val);
yecctoken2string({dot, _}) -> "'.'";
yecctoken2string({'$end', _}) ->
    [];
yecctoken2string({Other, _}) when is_atom(Other) ->
    io_lib:write(Other);
yecctoken2string(Other) ->
    io_lib:write(Other).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



-file("../tmp/elib1_parse_dtd.erl", 468).

yeccpars2(0=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(1=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_1(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(2=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(3=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(4=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(5=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(6=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(7=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(8=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(9=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(10=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_10(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(11=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(12=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_12(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(13=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(14=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(15=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(16=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(17=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_17(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(18=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(19=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(20=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_20(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(21=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(22=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_22(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(23=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(24=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(25=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(26=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_26(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(27=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(28=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_28(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(29=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_29(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(30=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_30(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(31=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(32=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_32(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(33=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(34=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(35=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(36=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(37=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(38=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(39=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(40=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(41=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_41(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(42=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(43=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(44=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(45=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(46=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_46(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(47=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(48=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(49=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(50=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_50(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(51=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_51(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(52=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(53=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(54=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_54(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(55=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_55(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(56=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_56(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(57=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(58=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(59=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(60=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(61=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_61(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(62=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_62(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.3",{missing_state_in_action_table, Other}}).

yeccpars2_0(S, attlist, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, element, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 4, Ss, Stack, T, Ts, Tzr).

yeccpars2_1(_S, '$end', _Ss, Stack,  _T, _Ts, _Tzr) ->
 {ok, hd(Stack)}.

yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_form(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_3(S, name, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 6, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_4: see yeccpars2_3

yeccpars2_5(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 7, Ss, Stack, T, Ts, Tzr);
yeccpars2_5(S, empty, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr).

yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_6_(Stack),
 yeccgoto_nameA(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_7(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, name, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 6, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, pcdata, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr).

yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_8_(Stack),
 yeccgoto_elementdecl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_9(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_9(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_9(S, '?', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_9_(Stack),
 yeccpars2_41(_S, Cat, [9 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_10(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_10(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_10_(Stack),
 yeccpars2_40(_S, Cat, [10 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_11(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr).

yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_content(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_13(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(S, name, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 6, Ss, Stack, T, Ts, Tzr).

yeccpars2_14(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_14_(Stack),
 yeccpars2_15(_S, Cat, [14 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_15_(Stack),
 yeccgoto_content(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_16: see yeccpars2_3

yeccpars2_17(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_17_(Stack),
 yeccgoto_mixed(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_18_(Stack),
 yeccgoto_pc1(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_19: see yeccpars2_3

yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_20_(Stack),
 yeccgoto_mixed(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_21(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_21(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_21(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_21_(Stack),
 yeccpars2_22(22, Cat, [21 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_22(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr).

yeccpars2_23(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(S, name, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 6, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_23_(Stack),
 yeccpars2_29(_S, Cat, [23 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_24(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_24(S, name, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 6, Ss, Stack, T, Ts, Tzr);
yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_24_(Stack),
 yeccpars2_26(_S, Cat, [24 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_25(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_25_(Stack),
 yeccgoto_choise(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_26_(Stack),
 yeccgoto_rest(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_27(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_27(S, name, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 6, Ss, Stack, T, Ts, Tzr);
yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_27_(Stack),
 yeccpars2_28(_S, Cat, [27 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_28_(Stack),
 yeccgoto_choise(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_29_(Stack),
 yeccgoto_rest(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_30(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_30_(Stack),
 yeccgoto_seq(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_31(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_31(S, name, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 6, Ss, Stack, T, Ts, Tzr);
yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_31_(Stack),
 yeccpars2_32(_S, Cat, [31 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_32_(Stack),
 yeccgoto_seq(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_33(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(S, '?', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_33_(Stack),
 yeccpars2_34(_S, Cat, [33 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_34_(Stack),
 yeccgoto_cp(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_35_(Stack),
 yeccgoto_mod(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_36_(Stack),
 yeccgoto_mod(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_37_(Stack),
 yeccgoto_mod(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_38(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(S, '?', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_38_(Stack),
 yeccpars2_39(_S, Cat, [38 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_39_(Stack),
 yeccgoto_elementdecl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_40_(Stack),
 yeccgoto_children(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_41_(Stack),
 yeccgoto_cp(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_42(S, name, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 6, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_42_(Stack),
 yeccpars2_45(_S, Cat, [42 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_43_(Stack),
 yeccgoto_elementdecl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_44(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, attype, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, entity, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr).

yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_45_(Stack),
 yeccgoto_attdefs(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_46(S, fixed, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, implied, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, required, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_46_(Stack),
 yeccpars2_55(_S, Cat, [46 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_47(S, name, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 6, Ss, Stack, T, Ts, Tzr);
yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_47_(Stack),
 yeccpars2_51(51, Cat, [47 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_48_(Stack),
 yeccgoto_theatttype(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_49_(Stack),
 yeccgoto_theatttype(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_50(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_50_(Stack),
 yeccgoto_enumeration(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_51(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr).

yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_52_(Stack),
 yeccgoto_theatttype(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_53(S, name, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 6, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_53_(Stack),
 yeccpars2_54(_S, Cat, [53 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_54_(Stack),
 yeccgoto_enumeration(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_defaultdecl(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_56(S, name, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 6, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_56_(Stack),
 yeccpars2_62(_S, Cat, [56 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_57(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_57_(Stack),
 yeccpars2_61(_S, Cat, [57 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_58_(Stack),
 yeccgoto_defaultdecl(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_59_(Stack),
 yeccgoto_defaultdecl(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_60_(Stack),
 yeccgoto_extra(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_61_(Stack),
 yeccgoto_defaultdecl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_62_(Stack),
 yeccgoto_attdef(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccgoto_attdef(42=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_attdef(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_attdefs(3=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_children(7=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_choise(24=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_choise(27=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_content(7, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(11, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_cp(7, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(10, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_cp(13, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(21, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_cp(23, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(30, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_cp(24, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(25, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_cp(27, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(25, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_cp(31, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(30, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_defaultdecl(46, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(56, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_elementdecl(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_enumeration(47, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_enumeration(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_extra(46=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_extra(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_form(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_mixed(16=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mixed(19=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_mod(9=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mod(33=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mod(38=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_nameA(3, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_nameA(4, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(5, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_nameA(7, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_nameA(13, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_nameA(16, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(17, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_nameA(19, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(17, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_nameA(23, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_nameA(24, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_nameA(27, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_nameA(31, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_nameA(42, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_nameA(47, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(50, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_nameA(53, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(50, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_nameA(56, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_pc1(14=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_rest(10=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rest(21, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(22, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_seq(23=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq(31=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_theatttype(44, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr).

-compile({inline,yeccpars2_6_/1}).
-file("elib1_parse_dtd.yrl", 104).
yeccpars2_6_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   fix_name ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_8_/1}).
-file("elib1_parse_dtd.yrl", 48).
yeccpars2_8_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { { element , __2 } , '$empty' }
  end | __Stack].

-compile({inline,yeccpars2_9_/1}).
-file("elib1_parse_dtd.yrl", 110).
yeccpars2_9_(__Stack0) ->
 [begin
   none
  end | __Stack0].

-compile({inline,yeccpars2_10_/1}).
-file("elib1_parse_dtd.yrl", 94).
yeccpars2_10_(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_14_/1}).
-file("elib1_parse_dtd.yrl", 80).
yeccpars2_14_(__Stack0) ->
 [begin
   '$pcdata'
  end | __Stack0].

-compile({inline,yeccpars2_15_/1}).
-file("elib1_parse_dtd.yrl", 76).
yeccpars2_15_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_17_/1}).
-file("elib1_parse_dtd.yrl", 82).
yeccpars2_17_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_18_/1}).
-file("elib1_parse_dtd.yrl", 79).
yeccpars2_18_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { '$mixed' , __2 }
  end | __Stack].

-compile({inline,yeccpars2_20_/1}).
-file("elib1_parse_dtd.yrl", 83).
yeccpars2_20_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_21_/1}).
-file("elib1_parse_dtd.yrl", 94).
yeccpars2_21_(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_23_/1}).
-file("elib1_parse_dtd.yrl", 98).
yeccpars2_23_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_24_/1}).
-file("elib1_parse_dtd.yrl", 102).
yeccpars2_24_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_25_/1}).
-file("elib1_parse_dtd.yrl", 100).
yeccpars2_25_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_26_/1}).
-file("elib1_parse_dtd.yrl", 93).
yeccpars2_26_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { '$alt' , __2 }
  end | __Stack].

-compile({inline,yeccpars2_27_/1}).
-file("elib1_parse_dtd.yrl", 102).
yeccpars2_27_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_28_/1}).
-file("elib1_parse_dtd.yrl", 101).
yeccpars2_28_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_29_/1}).
-file("elib1_parse_dtd.yrl", 92).
yeccpars2_29_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { '$seq' , __2 }
  end | __Stack].

-compile({inline,yeccpars2_30_/1}).
-file("elib1_parse_dtd.yrl", 97).
yeccpars2_30_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_31_/1}).
-file("elib1_parse_dtd.yrl", 98).
yeccpars2_31_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_32_/1}).
-file("elib1_parse_dtd.yrl", 96).
yeccpars2_32_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_33_/1}).
-file("elib1_parse_dtd.yrl", 110).
yeccpars2_33_(__Stack0) ->
 [begin
   none
  end | __Stack0].

-compile({inline,yeccpars2_34_/1}).
-file("elib1_parse_dtd.yrl", 90).
yeccpars2_34_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   fix_mod ( __5 , fix_type ( __2 , __3 ) )
  end | __Stack].

-compile({inline,yeccpars2_35_/1}).
-file("elib1_parse_dtd.yrl", 107).
yeccpars2_35_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   '$star'
  end | __Stack].

-compile({inline,yeccpars2_36_/1}).
-file("elib1_parse_dtd.yrl", 109).
yeccpars2_36_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   '$plus'
  end | __Stack].

-compile({inline,yeccpars2_37_/1}).
-file("elib1_parse_dtd.yrl", 108).
yeccpars2_37_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   '$question'
  end | __Stack].

-compile({inline,yeccpars2_38_/1}).
-file("elib1_parse_dtd.yrl", 110).
yeccpars2_38_(__Stack0) ->
 [begin
   none
  end | __Stack0].

-compile({inline,yeccpars2_39_/1}).
-file("elib1_parse_dtd.yrl", 51).
yeccpars2_39_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { { element , __2 } , fix_mod ( __6 , __4 ) }
  end | __Stack].

-compile({inline,yeccpars2_40_/1}).
-file("elib1_parse_dtd.yrl", 87).
yeccpars2_40_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   fix_type ( __1 , __2 )
  end | __Stack].

-compile({inline,yeccpars2_41_/1}).
-file("elib1_parse_dtd.yrl", 89).
yeccpars2_41_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   fix_mod ( __2 , __1 )
  end | __Stack].

-compile({inline,yeccpars2_42_/1}).
-file("elib1_parse_dtd.yrl", 58).
yeccpars2_42_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_43_/1}).
-file("elib1_parse_dtd.yrl", 52).
yeccpars2_43_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_45_/1}).
-file("elib1_parse_dtd.yrl", 54).
yeccpars2_45_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { { attlist , __1 } , __2 }
  end | __Stack].

-compile({inline,yeccpars2_46_/1}).
-file("elib1_parse_dtd.yrl", 74).
yeccpars2_46_(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_47_/1}).
-file("elib1_parse_dtd.yrl", 66).
yeccpars2_47_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_48_/1}).
-file("elib1_parse_dtd.yrl", 60).
yeccpars2_48_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   unwrap2 ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_49_/1}).
-file("elib1_parse_dtd.yrl", 61).
yeccpars2_49_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   entity
  end | __Stack].

-compile({inline,yeccpars2_50_/1}).
-file("elib1_parse_dtd.yrl", 65).
yeccpars2_50_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_52_/1}).
-file("elib1_parse_dtd.yrl", 62).
yeccpars2_52_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { '$enum' , __2 }
  end | __Stack].

-compile({inline,yeccpars2_53_/1}).
-file("elib1_parse_dtd.yrl", 66).
yeccpars2_53_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_54_/1}).
-file("elib1_parse_dtd.yrl", 64).
yeccpars2_54_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_56_/1}).
-file("elib1_parse_dtd.yrl", 58).
yeccpars2_56_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_57_/1}).
-file("elib1_parse_dtd.yrl", 74).
yeccpars2_57_(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_58_/1}).
-file("elib1_parse_dtd.yrl", 69).
yeccpars2_58_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   implied
  end | __Stack].

-compile({inline,yeccpars2_59_/1}).
-file("elib1_parse_dtd.yrl", 68).
yeccpars2_59_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   required
  end | __Stack].

-compile({inline,yeccpars2_60_/1}).
-file("elib1_parse_dtd.yrl", 73).
yeccpars2_60_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   unwrap3 ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_61_/1}).
-file("elib1_parse_dtd.yrl", 70).
yeccpars2_61_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { fixed , __2 }
  end | __Stack].

-compile({inline,yeccpars2_62_/1}).
-file("elib1_parse_dtd.yrl", 57).
yeccpars2_62_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ { __1 , { __2 , __3 } } | __4 ]
  end | __Stack].


-file("elib1_parse_dtd.yrl", 398).
