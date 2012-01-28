%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(elib1_xml_validate).
-compile(export_all).
-import(lists, [flatten/1, member/2, reverse/2]).

test() ->
    validate("../../lib/src/lorem.chap").

validate(File) ->
    case elib1_xml:parse_file(File) of
	{Tree, []} ->
	    DTD = get_dtd(Tree),
	    %% After this succeededs the atom table
	    %% contains all the atoms we need so we can convert the
	    %% string tag names in the tree to atoms when we do the testing
	    Node = get_data(Tree),
	    F = fun(Name, Ln, Top, Attrs, A) ->
			check(Name ,Ln, Top, Attrs, DTD, A)
		end,
	    %% io:format("Node=~p~n",[Node]),
	    fold_tree(Node, F, []);
	{_, L} ->
	    exit({syntax, L})
    end.

%% this is the checking function which is called for each node in the
%% tree - it is called by fold tree
%% A is an accumulated list of errors

check(Node,Line,Vals,Attrs,DTD,A) ->
    try list_to_existing_atom(Node) of
	Atom ->
	    case elib1_misc:lookup(Atom, DTD) of
		{ok, Defn} ->
		    Errs = isA(Atom, Line, Defn, Attrs, Vals),
		    reverse(Errs, A);
		error ->
		    [{node,Node,line,Line,notDefinedInDTD}|A]
	    end
    catch
	_:_ ->
	    [{node,Node,line,Line,notDefinedInDTD}|A]
    end.



%% isA -> [Errs]

isA(Name, Ln, {ElemDef,AttrDef}=_Def, Attrs, Val) ->
%%%     io:format("isA:~p inLine:~w def=~p~nattrs=~p~nval=~p~n",
%%% 	      [Name, Ln, _Def, Attrs, Val]),
    E1 = check_attributes(Name, AttrDef, Attrs),
    E2 = check_node(Name, ElemDef, Val),
    E = E1 ++ E2,
    io:format("E1,E2=~p ~p~n",[E1,E2]),
    case E of
	[] -> [];
	_  -> [{Name,Ln,E}]
    end.

check_attributes(_, error, []) -> [];
check_attributes(Name, {ok, Defs}, Vals) ->
    Errs = check_attributes(Defs, Vals),
    case Errs of
	[] ->
	    io:format("~p has great attributes hooray~n",[Name]),
	    [];
	_ ->
	    Errs
    end.

check_node(Name, Defs, Vals) ->
    %% io:format("Check name:~p Expect:~p~nfound:~p~n",
    %% 	      [Name, Defs, Vals]),
    try parse_node(Defs, Vals) of
	[] ->
	    io:format("** Hooray ~p is OK~n",[Name]),
	    [];
	[ws] ->
	    io:format("** Hooray ~p is OK~n",[Name]),
	    [];
	Tail ->
	    io:format("** Oh dear 1 ~p is Bad unexpected:~p~n",[Name,Tail]),
	    io:format("Defs=~p~nVals=~p~n",[Defs,Vals]),
	    [{unexpected,Tail,Name,Defs,Vals}]
    catch
	A:B ->
	    io:format("** Oh dear 2 ~p is Bad unexpected:~p~n",[Name,{A,B}]),
	    io:format("Defs=~p~nVals=~p~n",[Defs,Vals]),
	    [{unexpected,Name,A,B,Defs,Vals}]
    end.

%% parse_node(Defs, Vals) -> Vals'
%%   try to parse a node. Vals = [ws|{node,Name,Ln}|{pcdata,Ln}]
%%   returns the unconsumed Vals

parse_node('$pcdata', [{pcdata,_}|T]) -> T;
parse_node('$pcdata'=X, [ws|T]) -> parse_node(X, T);
parse_node(X, [ws|T]) -> parse_node(X, T);
parse_node('$empty', T) -> T;
parse_node({'$seq',L}, T) -> parse_seq(L, T);
parse_node({'$star',L}, T) -> parse_star(L, T);
parse_node({'$alt',L}, T) -> parse_alt(L, T);
parse_node({'$mixed',L}, T) -> parse_mixed(L, T);
parse_node({'$question',L}, T) -> parse_question(L, T);
parse_node({'$plus',L}, T) -> parse_plus(L, T);
parse_node(X, [ws|T]) when is_atom(X) ->
    %% looking for a tag, but found ws
    parse_node(X, T);
parse_node(N, [{node,N1,Ln}|T]) when is_atom(N) ->
    case N of
	N1 -> T;
	_ -> throw({expecting,N,found,N1,inLine,Ln})
    end;
parse_node(X, T) ->
    io:format("XXXXXXXXXXXXXXXXXXXXx=~p~n",[X]),
    throw({funny, X, T}).

parse_alt(L, [ws|T]) -> parse_alt(L, T);
parse_alt(L, [{node,N,Ln}|T]) ->
    case member(N, L) of
	true -> T;
	false -> throw({expecting,L,found,N,inLine,Ln})
    end;
parse_alt(L, [H|_]) ->
    throw({expecting,L,found,H}).

parse_mixed(_, [{pcdata,_}|T]) -> T;
parse_mixed(L, [ws|T]) -> parse_mixed(L, T);
parse_mixed(L, [{node,N,Ln}|T]) ->
    case member(N, L) of
	true -> T;
	false -> throw({expecting,L,orjust,'#pcdata',found,N,inLine,Ln})
    end;
parse_mixed(X, Y) ->
    io:format("UUUUUUUUUUUUUUUUUUU:~p ~p~n",[X,Y]),
    exit(ooops).

%% X? = 0 or 1

parse_question(X, [ws|T]) ->  parse_question(X, T);
parse_question(_, [])     ->  [];
parse_question(X, Vals)   ->
    %% zero or more
    %% io:format("parse_question:X=~p Vals=~p~n",[X,Vals]),
    try parse_node(X, Vals)
    catch
	throw:_ ->
	    Vals
    end.

%% plus 1 or more
parse_plus(X, [ws|T]) ->  parse_plus(X, T);
parse_plus(_, [])     ->  [];
parse_plus(X, Vals) ->
    try parse_node(X, Vals) of
	Vals1 ->
	    parse_star(X, Vals1)
    catch
	%% error
	throw:_ ->
	    exit({expecting,X,found,hd(Vals)})
    end.

parse_star(X, [ws|T]) ->  parse_star(X, T);
parse_star(_, []) ->  [];
parse_star(X, Vals) ->
    %% zero or more
    %% io:format("parse_star:X=~p Vals=~p~n",[X,Vals]),
    try parse_node(X, Vals) of
	Vals1 ->
	    parse_star(X, Vals1)
    catch
	throw:_ ->
	    Vals
    end.

parse_seq([H|T], Vals) -> parse_seq(T, parse_node(H, Vals));
parse_seq([], Vals)    -> Vals.

fold_tree({node,Name,Ln,Atts,Children}, F, A) ->
    Top = [top(I) || I <- Children],
    A1 = F(Name,Ln,Top,Atts,A),
    lists:foldl(fun(I, AA) -> fold_tree(I, F, AA) end, A1, Children);
fold_tree({raw,_,_,_}, _, A) ->
    %% this gets checked at the level above
    A.

top({node,N,Ln,_,_}) ->
    try list_to_existing_atom(N) of
	A -> {node, A, Ln}
    catch
	_:_ ->
	    exit({badTag,N,line,Ln})
    end;
top({raw,_,true,_})  -> ws;
top({raw,Ln,false,_}) -> {pcdata, Ln};
top(X) -> {other1,X}.


get_dtd([{doctype,_,Str}|_]) ->
    io:format("Str=~p~n",[Str]),
    case elib1_misc:string2toks(Str) of
	[{name,TopNameName},{name,"SYSTEM"},{str,File}] ->
	    get_sys_dtd(File)
    end;
get_dtd([_|T]) -> get_dtd(T).


get_data([{node,"chap",_,_,_}=X|_]) -> X;
get_data([_|T]) -> get_data(T).

get_sys_dtd(File) ->
    Full = os:getenv("HOME") ++ "/code/elib2-1/lib/src/" ++ File,
    io:format("Parsing:~p~n",[Full]),
    elib1_parse_dtd:parse_dtd_in_file(Full, fun(_,_) -> error end).

%%----------------------------------------------------------------------
%% check_attributes(Defs, Vals).

check_attributes(Defs, Vals) ->
    %% Check the required values
    E1 = [check_att(Status, Name,Type,Vals) || {Name, {Type,Status}} <- Defs],
    flatten(E1).

check_att(required, Name, Type, Vals) ->
    case elib1_misc:lookup(atom_to_list(Name), Vals) of
	{ok, Val} ->
	    check_value(Type, Val);
	error ->
	    [{missing,required,argument,Name}]
    end;
check_att(implied, Name, Type, Vals) ->
    case elib1_misc:lookup(atom_to_list(Name), Vals) of
	{ok, Val} ->
	    check_value(Type, Val);
	error ->
	    %% not an error if missing
	    []
    end;
check_att(Other, Name, Type, Vals) ->
    [{idontknowhowtocheckthis,Other,Name,Type,Vals}].

check_value(cdata, Str) -> [];
check_value(X,Y) -> [{idontknowhowtocheckthis,X,Y}].



