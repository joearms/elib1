%% elib1_parse_dtd   Parse an XML DTD
%% Time-stamp: <2009-09-22 21:21:44 joe>
%%---------------------------------------------------------------------------
%% Copyright (c) 2009 Joe Armstrong <erlang@gmail.com>
%% Copyright (c) 2009 Whoomph Software AB
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use, copy,
%% modify, merge, publish, distribute, sublicense, and/or sell copies
%% of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
%% BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
%% ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
%% CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
%%---------------------------------------------------------------------------
%% elib1_parse_dtd: Miscellaneous Functions
%% elib1_parse_dtd:parse/1 -- internal
%% elib1_parse_dtd:parse_dtd_in_file(File) -> [Form]
%% elib1_parse_dtd:parse_dtd_in_string(String) -> [Form]


%% Example forms:
%%
%% <!ELEMENT conf2 (Password?, log*, table*, FrontEndNode*, BackEndNode*,
%%                 FrontEndCluster*)>
%% <!ELEMENT Password (#PCDATA)>
%% <!ELEMENT table (name, type, access)>

Nonterminals
theatttype defaultdecl enumeration extra nameA
attdef attdefs rest pc1 form elementdecl content cp children choise mixed mod seq.

Terminals
empty attlist attype required fixed implied string entity
pcdata element name  '|' '(' ')' '*' '+' '?' ','.

Rootsymbol form.

form -> elementdecl: '$1'.

elementdecl -> element nameA empty: {{element, '$2'}, '$empty'}.

elementdecl -> element nameA '(' content ')' mod :
		   {{element,  '$2'}, fix_mod('$6', '$4')} .
elementdecl -> attlist attdefs: '$2'.

attdefs -> nameA attdef: {{attlist, '$1'},'$2'}.

attdef -> nameA theatttype defaultdecl attdef:
	      [{'$1',{'$2','$3'}}|'$4'].
attdef -> '$empty': [].

theatttype -> attype: unwrap2('$1').
theatttype -> entity: entity.
theatttype -> '(' enumeration ')': {'$enum','$2'}.

enumeration -> nameA '|' enumeration : ['$1'|'$3'].
enumeration -> nameA                 : ['$1'].
enumeration -> '$empty'              : [].

defaultdecl -> required: required.
defaultdecl -> implied: implied.
defaultdecl -> fixed extra : {fixed, '$2'}.
defaultdecl -> extra: '$1'.

extra -> string: unwrap3('$1').
extra -> '$empty': nil.

content	-> pcdata pc1   : '$2'.
content -> children     : '$1'.

pc1 -> '|' mixed: {'$mixed', '$2'}.
pc1 -> '$empty':  '$pcdata'.

mixed -> nameA : ['$1'].
mixed -> nameA '|' mixed: ['$1'|'$3'].

%% we've eaten up the '('

children ->  cp rest: fix_type('$1','$2').

cp -> nameA mod             : fix_mod('$2', '$1').
cp -> '(' cp rest ')' mod   : fix_mod('$5', fix_type('$2','$3')).

rest -> ',' seq    : {'$seq','$2'}.
rest -> '|' choise : {'$alt', '$2'}.
rest -> '$empty'   : nil.

seq -> cp ',' seq : ['$1'|'$3'].
seq -> cp         : ['$1'].
seq -> '$empty'  : [].

choise -> cp            : ['$1'].
choise -> cp '|' choise : ['$1'|'$3'].
choise -> '$empty'      : [].

nameA -> name: fix_name('$1').


mod -> '*' 			: '$star'.
mod -> '?' 			: '$question'.
mod -> '+' 			: '$plus'.
mod -> '$empty'                 : none.

Erlang code.

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
