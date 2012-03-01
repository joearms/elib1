%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

%% elib1_xml   XML stream parser
%% Time-stamp: <2010-12-03 19:34:19 joe>
%% This module implements an XML stream processor.

-module(elib1_xml).

%% % -compile(export_all).

-export([check_grammar/4,
	 parse_file/1,
	 parse_stream/1,
	 parse_string/1,
	 test_parse/0, test_tokenize/0, tokenize/3, token2str/1, parse/1]).

%% A completly abstract tokenizer.
%% It does not know where the characters come
%% from nor where they go to ...

%% An abstract parser that does not know

-import(lists, [foreach/2, member/2, reverse/1, reverse/2, sort/1]).

-define(in(X,Low,Hi), Low =< X, X =< Hi).          
-define(white(X), X==$ ; X==$\n ; X == $\t; X == $\r ).

%% Fun is a fun that knows how to get UTF32 charcters from some input stream
%% Fun() -> {[Chars], Fun'} | eos where Fun' is to be used the next time
%% round

%% @spec tokenize(F1, F2, State) -> State1

%% type F1() -> {Data, F1'} | eos where Data = [UTF32Chars]
%% type F2(Tag, State) -> State1

%%   F1 is called each time the input character list is exhaused.
%%   F2 is called each time a new Tag is found
%%   Finally State is returned

tokenize(F1, F2, State0) ->
    scan([], 1, F1, F2, State0).

scan([], Ln, F1, F2, State) ->
    case F1() of
	{Data, F1a} ->
	    scan(Data, Ln, F1a, F2, State);
	eos ->
	    State
    end;
scan(Str, Ln, F1, F2, State) ->
    case (catch get_next_token1(Str, Ln, F1)) of
	{'EXIT', Why} ->
	    io:format("Error in line ~p ~p~n[[~p]] ~n",[Ln, Why, Str]),
	    error;
	{ok, Tok, Str1, Ln1, F1a} ->
	    %% io:format("Tok=~p Str1=~p~n",[Tok, aStr1]),
	    State1 = F2(Tok, State),
	    scan(Str1, Ln1, F1a, F2, State1);
	eos ->
	    State
    end.

%% % bump_ln($\n, N) -> 1 + N;
%% % bump_ln(_, N)   -> N.

%% % -spec get_next_token1(string(), Line::integer()) ->
%% %   {ok, token(), Str1::string(), Ln1::integer()} | {error, any()} | more.

get_next_token1("<?" ++ T, Ln, F)     -> get_pi(T, [],  Ln, F);
get_next_token1("<!DOCTYPE" ++ T, Ln, F1) -> get_doctype(T, Ln, F1);
get_next_token1("<![CDATA[" ++ T, Ln, F1) -> get_cdata(T, [], Ln, F1);
get_next_token1("<!--" ++ T, Ln, F1)      -> get_comment(T, [], Ln, F1);
get_next_token1("<!" = X, Ln, F1)         -> get_next_token2(X, Ln, F1);
get_next_token1("<!-" = X, Ln, F1)        -> get_next_token2(X, Ln, F1);
get_next_token1("<!D" = X, Ln, F1)        -> get_next_token2(X, Ln, F1);
get_next_token1("<!DO" = X, Ln, F1)       -> get_next_token2(X, Ln, F1);
get_next_token1("<!DOC" = X, Ln, F1)      -> get_next_token2(X, Ln, F1);
get_next_token1("<!DOCT" = X, Ln, F1)     -> get_next_token2(X, Ln, F1);
get_next_token1("<!DOCTY" = X, Ln, F1)    -> get_next_token2(X, Ln, F1);
get_next_token1("<!DOCTYP" = X, Ln, F1)   -> get_next_token2(X, Ln, F1);
get_next_token1("<![" = X, Ln, F1)        -> get_next_token2(X, Ln, F1);
get_next_token1("<![C" = X, Ln, F1)       -> get_next_token2(X, Ln, F1);
get_next_token1("<![CD" = X, Ln, F1)      -> get_next_token2(X, Ln, F1);
get_next_token1("<![CDA" = X, Ln, F1)     -> get_next_token2(X, Ln, F1);
get_next_token1("<![CDAT" = X, Ln, F1)    -> get_next_token2(X, Ln, F1);
get_next_token1("<![CDATA" = X, Ln, F1)   -> get_next_token2(X, Ln, F1);
get_next_token1("<" = X, Ln, F1)          -> get_next_token2(X, Ln, F1);
get_next_token1("<!" ++ _, Ln, _) -> 
    error(Ln,"expecting comment, DOCTYPE or CDATA");
get_next_token1("<" ++ T, Ln, F1) -> get_tag(T, Ln, F1);
get_next_token1(eos, _Ln, _F1)    -> eos;
get_next_token1(S, Ln, F1)        -> get_CharData(S, Ln, F1).

%% get_next_token2+ is called when we have insufficient data in 
%% Str

get_next_token2(Str, Ln, F1) ->
    case F1() of
	{Data, F1a} -> get_next_token1(Str ++ Data, Ln, F1a);
	eos         -> eos
    end.

%% % <!DOCTYPE ... > Skip nested < > and skip over " "
%% %                 and '..'
%% % <!--      .. -->
%% % <![CDATA[ .. ]]>

%% PI's cannot contain $>$? (even inside quotes)

get_pi(">"++T, "?"++L, Ln, F1) -> {ok, {pi, Ln, reverse(L)}, T, Ln, F1};
get_pi([$\n|T], L, Ln, F1)     -> get_pi(T, [$\n|L], Ln+1, F1);
get_pi([H|T], L, Ln, F1)       -> get_pi(T, [H|L], Ln, F1);
get_pi([], L, Ln, F1)          ->
    case F1() of
	{Str, F1a} -> get_pi(Str, L, Ln, F1a);
	eos        -> error(Ln,"eof in PI")
    end.

%% % ----------------------------------------------------------------------
%% Comments 

get_comment(">"++T,"--"++L,Ln,F1) -> {ok, {comment,Ln,reverse(L)}, T, Ln, F1};
get_comment([$\n|T], L, Ln, F1)   -> get_comment(T, [$\n|L], Ln+1, F1);
get_comment([H|T], L, Ln, F1)     -> get_comment(T, [H|L], Ln, F1);
get_comment([], L, Ln, F1)        ->
    case F1() of
	{Data, F1a} -> get_comment(Data, L, Ln, F1a);
	eos         -> error(Ln, "eof in comment")
    end.
    
%% % ----------------------------------------------------------------------
%% CDATA 

get_cdata(">" ++ T, "]]"++L, Ln, F1) -> {ok, {cdata, Ln,reverse(L)}, T, Ln, F1};
get_cdata([$\n|T], L, Ln, F1)        -> get_cdata(T, [$\n|L], Ln+1, F1);
get_cdata([H|T], L, Ln, F1)          -> get_cdata(T, [H|L], Ln, F1);
get_cdata([], L, Ln, F1)             -> 
    case F1() of
	{Data, F1a} -> get_cdata(Data, L, Ln, F1a);
	eos         -> error(Ln, "eof in CDATA")
    end.

%% % ----------------------------------------------------------------------
%% DOCTYPE --
%%   To fetch a doctype we skip " " (always)
%%   and nest naked "$<$" "$>$" 's

get_doctype(Str, Ln, F1) ->
    get_doctype(Str, [], 0, Ln, F1).

get_doctype([$"|T], L, Level, Ln, F1) ->
    get_q_doctype($", T, [$"|L], Level, Ln, F1);
get_doctype([$'|T], L, Level, Ln, F1) ->
    get_q_doctype($', T, [$'|L], Level, Ln, F1);
get_doctype([$>|T], L, 0, Ln, F1) ->
    {ok, {doctype, Ln, reverse(L)}, T, Ln, F1};
get_doctype([$>|T], L, Level, Ln, F1) ->
    get_doctype(T, [$>|L], Level-1, Ln, F1);
get_doctype([$<|T], L, Level, Ln, F1) ->
    get_doctype(T, [$<|L], Level+1, Ln, F1);
get_doctype([$\n|T], L, Level, Ln, F1) ->
    get_doctype(T, [$\n|L], Level, Ln+1, F1);
get_doctype([H|T], L, Level, Ln, F1) ->
    get_doctype(T, [H|L], Level, Ln, F1);
get_doctype([], L, Level, Ln, F1) ->
    case F1() of
	{Str, F1a} -> get_doctype(Str, L, Level, Ln, F1a);
	eos        -> error(Ln,"eos in doctype")
    end.

get_q_doctype(Stop, [Stop|T], L, Level, Ln, F1) ->
    get_doctype(T, [Stop|L], Level, Ln, F1);
get_q_doctype(Stop, [$\n|T], L, Level, Ln, F1) ->
    get_q_doctype(Stop, T, [$\n|L], Level, Ln+1, F1);
get_q_doctype(Stop, [H|T], L, Level, Ln, F1) ->
    get_q_doctype(Stop, T, [H|L], Level, Ln, F1);
get_q_doctype(Stop, [], L, Level, Ln, F1) ->
    case F1() of
	{Str, F1a} -> get_q_doctype(Stop, Str, L, Level, Ln, F1a);
	eos        -> error(Ln, "eos in doctype")
    end.

%% Cut from:  REC-xml-19980210
%%  http://www.w3.org/TR/REC-xml/#sec-cdata-sect
%% 
%% [39] element      ::= EmptyElemTag | STag content ETag
%% [40] STag         ::= '<' Name (S Attribute)* S? '>'
%  [42] ETag         ::= '</' Name S? '>'
%% [4]  NameChar     ::= Letter | Digit | '.' | '-' | '_' | ':' | CombiningChar
%%                     | Extender
%% [5]  Name         ::= (Letter | '_' | ':') (NameChar)*
%% [41] Attribute    ::= Name Eq AttValue
%% [25] Eq           ::= S? '=' S?
%% [10] AttValue     ::= '"' ([^<&"] | Reference)* '"'
%%                     |  "'" ([^<&'] | Reference)* "'"
%% [44] EmptyElemTag ::= '<' Name (S Attribute)* S? '/>'
%% [40] STag         ::= '<' Name (S Attribute)* S? '>'
%% [41] Attribute    ::= Name Eq AttValue
%% [25] Eq           ::= S? '=' S?
%% [84] Letter	     ::= BaseChar | Ideographic  
%% [85] BaseChar     ::= [#x0041-#x005A] | .. etc.. (see later)
%% [86]	Ideographic  ::= [#x4E00-#x9FA5] | #x3007 | [#x3021-#x3029] 
%% 
%% [87] CombiningChar ::= [#x0300-#x0345] | [#x0360-#x0361] ..etc... see later
%% [88]	Digit	      ::= [#x0030-#x0039] .. etc ... see later
%% [89]	Extender      ::= #x00B7 .. etc .. see later
%% [67] Reference     ::=  EntityRef | CharRef
%% [68] EntityRef     ::=  '&' Name <#NT-Name> ';'
%% [69] PEReference   ::=  '%' Name <#NT-Name> ';'
%% [43]	content	      ::=  CharData? ((element | Reference | CDSect | PI | 
%%                         Comment) CharData?)*
%% [14] CharData      ::=  [^<&]* - ([^<&]* ']]>' [^<&]*)
%% 
%%
%% reent_test breaks the input string at all possible places
%% and checks that the parser correctly re-enters.


%% % ----------------------------------------------------------------------
%% In parse_tag we know that we have 
%% enough characters for a complete
%% parse so the code does not have to be re-entrant

%% get tag is called after we have seen "$<$" 

get_tag("/" ++ S, Ln, F1) ->
    {Name, S1, F1a} = get_Name(S, [], F1),
    {S2, Ln2, F1b}  = skip_white(S1, Ln, F1a),
    case S2 of
	">" ++ S3->
	    {ok, {eTag, Ln, Name}, S3, Ln2, F1b};
	_ ->
	    error(Ln, "expecting >")
    end;
get_tag([], Ln, F1) ->
    case F1() of
	{Str, F1a} -> get_tag(Str, Ln, F1a);
	eos        -> error(Ln, "eos in tag")
    end;
get_tag(S, Ln, F1) ->
    {Name, S1, F1a}      = get_Name(S, Ln, F1),
    {Args, S2, Ln1, F1b} = get_args(S1, [], Ln, F1a),
    %% after calling get_args S2 might be only one character long
    %% so we must be rather careful here ...
    after_get_args(S2, Ln1, Ln, Name, Args, F1b).

after_get_args("/", Ln1, Ln, Name, Args, F1) ->
    %% not enough data
    {Str, F1a} = F1(),
    case Str of
	eof ->
	    error(Ln1, "illegal eof after /");
	_ ->
	    after_get_args("/" ++ Str, Ln1, Ln, Name, Args, F1a)
    end;
after_get_args("/>" ++ S3,  Ln1, Ln, Name, Args, F1) ->
    {ok, {empty, Ln, Name, Args}, S3, Ln1, F1};
after_get_args(">" ++ S3,   Ln1, Ln, Name, Args, F1) ->
    {ok, {sTag, Ln, Name, Args}, S3, Ln1, F1};
after_get_args([], Ln1, Ln, Name, Args, F1) ->
    case F1() of
	{Str, F1a} -> after_get_args(Str, Ln1, Ln, Name, Args, F1a);
	eos        -> error(Ln, "expecting /> or >")
    end.

%% [5]  Name  ::= (Letter | '_' | ':') (NameChar)*

%% % -spec get_Name(string()) -> {Name::string(), Str1::string()}.

get_Name([H|T], Ln, F1) ->
    case is_Name_start(H) of
	true  -> get_NameChars(T, [H], Ln, F1);
	false -> error(Ln, "Bad character (expecting start of name):" ++ [H])
    end;
get_Name([], Ln, F1) ->
    case F1() of
	{Data, F1a} -> get_Name(Data, Ln, F1a);
	eos         -> error(Ln, "eof in Name")
    end.

is_Name_start($_) -> true;
is_Name_start($:) -> true;
is_Name_start(X)  -> is_Letter(X).

%% [84] Letter	     ::= BaseChar | Ideographic

is_Letter(X) -> is_BaseChar(X) orelse is_Ideographic(X).

%% We are inside the Name
%% collects NameChar*

get_NameChars([H|T]=S, L, Ln, F1) ->
    case is_NameChar(H) of
	true  -> get_NameChars(T, [H|L], Ln, F1);
	false -> {reverse(L), S, F1}
    end;
get_NameChars([], L, Ln, F1) ->
    case F1() of 
	{Data, F1a} -> get_NameChars(Data, L, Ln, F1a);
	eos         -> error(Ln, "eos in Name")
    end.

%% [4]  NameChar     ::= Letter | Digit | '.' | '-' | '_' | ':' | CombiningChar
%%                     | Extender

is_NameChar($.) -> true;
is_NameChar($-) -> true;
is_NameChar($_) -> true;
is_NameChar($:) -> true;
is_NameChar(X)  -> 
    is_Letter(X) orelse is_Digit(X) orelse is_CombiningChar(X) 
	orelse is_Extender(X).

%% get_args collects the (S Attribute)* bit

%% [40] STag         ::= '<' Name (S Attribute)* S? '>'

get_args(S, L, Ln, F1) ->
    {S1, Ln1, F1a} = skip_white(S, Ln, F1),
    %% after calling skip_white S is eof or not nil
    case S1 of
	[$/|_] ->
	    {sort(L), S1, Ln1, F1a};
	[$>|_] ->
	    {sort(L), S1, Ln1, F1a};
	_ ->
	    %% Expecting an attribute
	    %% [41] Attribute    ::= Name Eq AttValue
	    {Name, S2, F1b} = get_Name(S1, Ln1, F1a),
	    {S3, Ln3, F1c}  = skip_white(S2, Ln1, F1b),
	    case S3 of
		"=" ++ S4 ->
		    {S5, Ln5, F1d} = skip_white(S4, Ln3, F1c),
		    {Val, S6, Ln6, F1e} = get_AttValue(S5, Ln5, F1d),
		    get_args(S6, [{Name,Val}|L], Ln6, F1e);
		_ ->
		    error(Ln3, "expecting =")
	    end
    end.


%% [10] AttValue     ::= '"' ([^<&"] | Reference)* '"'
%%                     |  "'" ([^<&'] | Reference)* "'"

get_AttValue([$"|T], Ln, F1) -> collect_AttVal($", T, [], Ln, F1);
get_AttValue([$'|T], Ln, F1) -> collect_AttVal($', T, [], Ln, F1);
get_AttValue([_|_], Ln, _)  -> error(Ln, "expecting ' or \"");
get_AttValue([], Ln, F1)     -> 
    case F1() of
	{Data, F1a} -> get_AttValue(Data, Ln, F1a);
	eos         -> error(Ln, "eof in attribute value")
    end.

collect_AttVal(S, [S|T], L, Ln, F1)   -> {reverse(L), T, Ln, F1};
collect_AttVal(S, [$\n|T], L, Ln, F1) -> collect_AttVal(S, T, [$\n|L], Ln+1, F1);
collect_AttVal(_, [$<|_], _, Ln, _)  -> error(Ln, "< in attribute value");
collect_AttVal(S, [$&|_]=T, L, Ln, F1)  -> 
    {Ref, T1, F1a} = get_reference(T, Ln, F1),
    collect_AttVal(S, T1, add_ref(Ref, L), Ln, F1a);
collect_AttVal(S, [$%|_]=T, L, Ln, F1)  -> 
    {Ref, T1, F1a} = get_reference(T, Ln, F1),
    collect_AttVal(S, T1, add_ref(Ref, L), Ln, F1a);
collect_AttVal(S, [H|T], L, Ln, F1)   -> collect_AttVal(S, T, [H|L], Ln, F1);
collect_AttVal(S, [], L, Ln, F1)      -> 
    case F1() of
	{Data, F1a} -> collect_AttVal(S, Data, L, Ln, F1a);
	eos         -> error(Ln, "eos in attribute")
    end.

%% [67] Reference     ::=  EntityRef | CharRef
%% [68] EntityRef     ::=  '&' Name ';'
%% [69] PEReference   ::=  '%' Name ';'

%% I've added a bit to collect entities like &#x...;

get_reference("&#" ++ T, Ln, F1) ->
    {Val, T1, F1} = get_hex_entity(T, [], Ln, F1),
    {{entity, Val}, T1, F1}; 
get_reference("&" ++ T, Ln, F1) ->
    {Name, T1, F1a} = get_Name(T, Ln, F1),
    case T1 of
	";" ++ T2 -> {{entityRef, Name}, T2, F1a};
	_ -> error(Ln, "bad entity ref")
    end;
get_reference("%" ++ T, Ln, F1) ->
    {Name, T1, F1a} = get_Name(T, Ln, F1),
    case T1 of
	";" ++ T2 -> {{parameterRef, Name}, T2, F1a};
	_ -> error(Ln, "bad Parameter ref")
    end;
get_reference([], Ln, F1) ->
    case F1() of
	{Data, F1a} -> get_reference(Data, Ln, F1a);
	eos         -> error(Ln, "eof in reference")
    end.

get_hex_entity(";" ++ T, L, _Ln, F) ->
    {reverse(L), T, F};
get_hex_entity("\n" ++ _, _L, Ln, _F) ->
    error(Ln, "nl in entity");
get_hex_entity([H|T], L, Ln, F) ->
    get_hex_entity(T, [H|L], Ln, F);
get_hex_entity([], L, Ln, F1) ->
    case F1() of
	{Data, F1a} -> get_hex_entity(Data, L, Ln, F1a);
	eos         -> error(Ln, "eof in reference")
    end.

skip_white([$\s|T], Ln, F1)  -> skip_white(T, Ln, F1);
skip_white([$\t|T], Ln, F1)  -> skip_white(T, Ln, F1);
skip_white([$\n|T], Ln, F1)  -> skip_white(T, Ln+1, F1);
skip_white([13|T], Ln, F1)   -> skip_white(T, Ln+1, F1);
skip_white([], Ln, F1)       -> 
    case F1() of
	{Data, F1a} -> skip_white(Data, Ln, F1a);
	eos         -> {[], Ln, F1}
    end;
skip_white(Str, Ln, F1)      -> {Str, Ln, F1}.
 
%% % is_MiscName($.) -> true;
%% % is_MiscName($-) -> true;
%% % is_MiscName($_) -> true;
%% % is_MiscName($:) -> true;
%% % is_MiscName(_)  -> false.
 
%% get_CharData (this is a tail call from the top level)

get_CharData(Str, Ln, F1) -> get_CharData(Str, [], Ln, F1).

get_CharData([$<|_] = T, L, Ln, F1) -> found_raw(1, T, L, Ln, F1);
get_CharData([$\n|T], L, Ln, F1)    -> get_CharData(T, [$\n|L], Ln+1, F1);
get_CharData([$\r|T], L, Ln, F1)    -> get_CharData(T, [$\r|L], Ln, F1);
get_CharData([$&|_]=T, L, Ln, F1)   -> get_amp(T, L, Ln, F1);
get_CharData([H|T], L, Ln, F1)      -> get_CharData(T, [H|L], Ln, F1);
get_CharData([], L, Ln, F1)         -> 
    case F1() of
	{Data, F1a} -> get_CharData(Data, L, Ln, F1a);
	eos         -> found_raw(2, eos, L, Ln, F1)
    end.

get_amp(T, L, Ln, F1) ->
    {Ref, T1, F1a} = get_reference(T, Ln, F1),
    get_CharData(T1, add_ref(Ref,L), Ln, F1a).

found_raw(K, T, L, Ln, F1) ->
    case L of "" ->
	    io:format("************************* emptyraw ~p ~p~n",[K,Ln]);
	_ -> true
    end,
    Blank = all_blank(L),
    {ok, {raw,Ln-count_nls(L,0),Blank,reverse(L)}, T, Ln, F1}.

%% add_ref({entityRef, "lt"}, L) -> [$<|L];
%% add_ref({entityRef, "amp"}, L) -> [$&|L];
%% add_ref({entityRef, "gt"}, L) -> [$>|L];
%% add_ref({entityRef, "quot"}, L) -> [$"|L];
add_ref({entityRef, H}, L) ->
    reverse("&" ++ H ++";", L);
add_ref(X, L) ->
    io:format("add_ref:~p~n",[X]),
    [X|L].

%% count nls is done *after* conversion from utf8 to utf32

count_nls([$\n|T], N) -> count_nls(T, N+1);
count_nls([_|T], N)   -> count_nls(T, N);
count_nls([], N)      -> N.

error(Ln, Term) ->
    erlang:error({Ln, Term}).

token2str({eTag,_,Tag})       -> ["</",Tag,">"];
token2str({raw,_,_,Str})      -> Str;
token2str({special,_,Str})    -> Str;
token2str({sTag,_,Tag,Args})  -> ["<",Tag,pargs(Args),">"];
token2str({empty,_,Tag,Args}) -> ["<",Tag,pargs(Args),"/>"];
token2str({doctype,_,Str})    -> ["<!DOCTYPE",Str,">"];
token2str({pi,_,Str})         -> ["<?",Str,"?>"];
token2str({comment,_,Str})    -> ["<!--",Str,"-->"];
token2str({cdata,_,Str})      -> ["<![CDATA[", Str, "]]>"].

pargs([])            -> [];
pargs([{Key,Val}|T]) -> [$\s,Key,$=,$",Val,$"|pargs(T)].


all_blank([]) -> true;
all_blank([$\r|T]) -> all_blank(T);
all_blank([$\n|T]) -> all_blank(T);
all_blank([$\s|T]) -> all_blank(T);
all_blank([$\t|T]) -> all_blank(T);
all_blank(_) -> false.

%% these are taken from appendix B of
%% http://www.w3.org/TR/REC-xml/#NT-Letter

%% [85] |BaseChar|	   ::=   	|[#x0041-#x005A] | [#x0061-#x007A]
%% | [#x00C0-#x00D6] | [#x00D8-#x00F6] | [#x00F8-#x00FF] | [#x0100-#x0131]
%% ...
%% | [#x0134-#x013E] | [#x0141-#x0148] | [#x014A-#x017E] | [#x0180-#x01C3]
%% | [#x01CD-#x01F0] | [#x01F4-#x01F5] | [#x01FA-#x0217] | [#x0250-#x02A8]
%% | [#x02BB-#x02C1] | #x0386 | [#x0388-#x038A] | #x038C | [#x038E-#x03A1]
%% | [#x03A3-#x03CE] | [#x03D0-#x03D6] | #x03DA | #x03DC | #x03DE | #x03E0
%% | [#x03E2-#x03F3] | [#x0401-#x040C] | [#x040E-#x044F] | [#x0451-#x045C]
%% | [#x045E-#x0481] | [#x0490-#x04C4] | [#x04C7-#x04C8] | [#x04CB-#x04CC]
%% | [#x04D0-#x04EB] | [#x04EE-#x04F5] | [#x04F8-#x04F9] | [#x0531-#x0556]
%% | #x0559 | [#x0561-#x0586] | [#x05D0-#x05EA] | [#x05F0-#x05F2]
%% | [#x0621-#x063A] | [#x0641-#x064A] | [#x0671-#x06B7] | [#x06BA-#x06BE]
%% | [#x06C0-#x06CE] | [#x06D0-#x06D3] | #x06D5 | [#x06E5-#x06E6]
%% | [#x0905-#x0939] | #x093D | [#x0958-#x0961] | [#x0985-#x098C]
%% | [#x098F-#x0990] | [#x0993-#x09A8] | [#x09AA-#x09B0] | #x09B2
%% | [#x09B6-#x09B9] | [#x09DC-#x09DD] | [#x09DF-#x09E1] | [#x09F0-#x09F1]
%% | [#x0A05-#x0A0A] | [#x0A0F-#x0A10] | [#x0A13-#x0A28] | [#x0A2A-#x0A30]
%% | [#x0A32-#x0A33] | [#x0A35-#x0A36] | [#x0A38-#x0A39] | [#x0A59-#x0A5C]
%% | #x0A5E | [#x0A72-#x0A74] | [#x0A85-#x0A8B] | #x0A8D | [#x0A8F-#x0A91]
%% | [#x0A93-#x0AA8] | [#x0AAA-#x0AB0] | [#x0AB2-#x0AB3] | [#x0AB5-#x0AB9]
%% | #x0ABD | #x0AE0 | [#x0B05-#x0B0C] | [#x0B0F-#x0B10] | [#x0B13-#x0B28]
%% | [#x0B2A-#x0B30] | [#x0B32-#x0B33] | [#x0B36-#x0B39] | #x0B3D
%% | [#x0B5C-#x0B5D] | [#x0B5F-#x0B61] | [#x0B85-#x0B8A] | [#x0B8E-#x0B90]
%% | [#x0B92-#x0B95] | [#x0B99-#x0B9A] | #x0B9C | [#x0B9E-#x0B9F]
%% | [#x0BA3-#x0BA4] | [#x0BA8-#x0BAA] | [#x0BAE-#x0BB5] | [#x0BB7-#x0BB9]
%% | [#x0C05-#x0C0C] | [#x0C0E-#x0C10] | [#x0C12-#x0C28] | [#x0C2A-#x0C33]
%% | [#x0C35-#x0C39] | [#x0C60-#x0C61] | [#x0C85-#x0C8C] | [#x0C8E-#x0C90]
%% | [#x0C92-#x0CA8] | [#x0CAA-#x0CB3] | [#x0CB5-#x0CB9] | #x0CDE
%% | [#x0CE0-#x0CE1] | [#x0D05-#x0D0C] | [#x0D0E-#x0D10] | [#x0D12-#x0D28]
%% | [#x0D2A-#x0D39] | [#x0D60-#x0D61] | [#x0E01-#x0E2E] | #x0E30
%% | [#x0E32-#x0E33] | [#x0E40-#x0E45] | [#x0E81-#x0E82] | #x0E84
%% | [#x0E87-#x0E88] | #x0E8A | #x0E8D | [#x0E94-#x0E97] | [#x0E99-#x0E9F]
%% | [#x0EA1-#x0EA3] | #x0EA5 | #x0EA7 | [#x0EAA-#x0EAB] | [#x0EAD-#x0EAE]
%% | #x0EB0 | [#x0EB2-#x0EB3] | #x0EBD | [#x0EC0-#x0EC4] | [#x0F40-#x0F47]
%% | [#x0F49-#x0F69] | [#x10A0-#x10C5] | [#x10D0-#x10F6] | #x1100
%% | [#x1102-#x1103] | [#x1105-#x1107] | #x1109 | [#x110B-#x110C]
%% | [#x110E-#x1112] | #x113C | #x113E | #x1140 | #x114C | #x114E | #x1150
%% | [#x1154-#x1155] | #x1159 | [#x115F-#x1161] | #x1163 | #x1165 | #x1167
%% | #x1169 | [#x116D-#x116E] | [#x1172-#x1173] | #x1175 | #x119E | #x11A8
%% | #x11AB | [#x11AE-#x11AF] | [#x11B7-#x11B8] | #x11BA | [#x11BC-#x11C2]
%% | #x11EB | #x11F0 | #x11F9 | [#x1E00-#x1E9B] | [#x1EA0-#x1EF9]
%% | [#x1F00-#x1F15] | [#x1F18-#x1F1D] | [#x1F20-#x1F45] | [#x1F48-#x1F4D]
%% | [#x1F50-#x1F57] | #x1F59 | #x1F5B | #x1F5D | [#x1F5F-#x1F7D]
%% | [#x1F80-#x1FB4] | [#x1FB6-#x1FBC] | #x1FBE | [#x1FC2-#x1FC4]
%% | [#x1FC6-#x1FCC] | [#x1FD0-#x1FD3] | [#x1FD6-#x1FDB] | [#x1FE0-#x1FEC]
%% | [#x1FF2-#x1FF4] | [#x1FF6-#x1FFC] | #x2126 | [#x212A-#x212B] | #x212E
%% | [#x2180-#x2182] | [#x3041-#x3094] | [#x30A1-#x30FA] | [#x3105-#x312C]
%% | [#xAC00-#xD7A3] |
%% [86]   	|Ideographic|	   ::=   	|[#x4E00-#x9FA5] | #x3007
%% | [#x3021-#x3029] |
%% [87]   	|CombiningChar|	   ::=   	|[#x0300-#x0345] | [#x0360-#x0361]
%% | [#x0483-#x0486] | [#x0591-#x05A1] | [#x05A3-#x05B9] | [#x05BB-#x05BD]
%% | #x05BF | [#x05C1-#x05C2] | #x05C4 | [#x064B-#x0652] | #x0670
%% | [#x06D6-#x06DC] | [#x06DD-#x06DF] | [#x06E0-#x06E4] | [#x06E7-#x06E8]
%% | [#x06EA-#x06ED] | [#x0901-#x0903] | #x093C | [#x093E-#x094C] | #x094D
%% | [#x0951-#x0954] | [#x0962-#x0963] | [#x0981-#x0983] | #x09BC | #x09BE
%% | #x09BF | [#x09C0-#x09C4] | [#x09C7-#x09C8] | [#x09CB-#x09CD] | #x09D7
%% | [#x09E2-#x09E3] | #x0A02 | #x0A3C | #x0A3E | #x0A3F | [#x0A40-#x0A42]
%% | [#x0A47-#x0A48] | [#x0A4B-#x0A4D] | [#x0A70-#x0A71] | [#x0A81-#x0A83]
%% | #x0ABC | [#x0ABE-#x0AC5] | [#x0AC7-#x0AC9] | [#x0ACB-#x0ACD]
%% | [#x0B01-#x0B03] | #x0B3C | [#x0B3E-#x0B43] | [#x0B47-#x0B48]
%% | [#x0B4B-#x0B4D] | [#x0B56-#x0B57] | [#x0B82-#x0B83] | [#x0BBE-#x0BC2]
%% | [#x0BC6-#x0BC8] | [#x0BCA-#x0BCD] | #x0BD7 | [#x0C01-#x0C03]
%% | [#x0C3E-#x0C44] | [#x0C46-#x0C48] | [#x0C4A-#x0C4D] | [#x0C55-#x0C56]
%% | [#x0C82-#x0C83] | [#x0CBE-#x0CC4] | [#x0CC6-#x0CC8] | [#x0CCA-#x0CCD]
%% | [#x0CD5-#x0CD6] | [#x0D02-#x0D03] | [#x0D3E-#x0D43] | [#x0D46-#x0D48]
%% | [#x0D4A-#x0D4D] | #x0D57 | #x0E31 | [#x0E34-#x0E3A] | [#x0E47-#x0E4E]
%% | #x0EB1 | [#x0EB4-#x0EB9] | [#x0EBB-#x0EBC] | [#x0EC8-#x0ECD]
%% | [#x0F18-#x0F19] | #x0F35 | #x0F37 | #x0F39 | #x0F3E | #x0F3F
%% | [#x0F71-#x0F84] | [#x0F86-#x0F8B] | [#x0F90-#x0F95] | #x0F97
%% | [#x0F99-#x0FAD] | [#x0FB1-#x0FB7] | #x0FB9 | [#x20D0-#x20DC] | #x20E1
%% | [#x302A-#x302F] | #x3099 | #x309A |
%% [88]   	|Digit|	   ::=   	|[#x0030-#x0039] | [#x0660-#x0669]
%% | [#x06F0-#x06F9] | [#x0966-#x096F] | [#x09E6-#x09EF] | [#x0A66-#x0A6F]
%% | [#x0AE6-#x0AEF] | [#x0B66-#x0B6F] | [#x0BE7-#x0BEF] | [#x0C66-#x0C6F]
%% | [#x0CE6-#x0CEF] | [#x0D66-#x0D6F] | [#x0E50-#x0E59] | [#x0ED0-#x0ED9]
%% | [#x0F20-#x0F29] |
%% [89]   	|Extender|	   ::=   	|#x00B7 | #x02D0 | #x02D1 | #x0387 | #x0640
%% | #x0E46 | #x0EC6 | #x3005 | [#x3031-#x3035] | [#x309D-#x309E]
%% | [#x30FC-#x30FE] |

is_BaseChar(X) when ?in(X, 16#0041, 16#005A) -> true; 
is_BaseChar(X) when ?in(X, 16#0061, 16#007A) -> true; 
is_BaseChar(X) when ?in(X, 16#00C0, 16#00D6) -> true; 
is_BaseChar(X) when ?in(X, 16#00D8, 16#00F6) -> true; 
is_BaseChar(X) when ?in(X, 16#00F8, 16#00FF) -> true; 
is_BaseChar(X) when ?in(X, 16#0100, 16#0131) -> true; 
is_BaseChar(X) when ?in(X, 16#0134, 16#013E) -> true; 
is_BaseChar(X) when ?in(X, 16#0141, 16#0148) -> true; 
is_BaseChar(X) when ?in(X, 16#014A, 16#017E) -> true; 
is_BaseChar(X) when ?in(X, 16#0180, 16#01C3) -> true; 
is_BaseChar(X) when ?in(X, 16#01CD, 16#01F0) -> true; 
is_BaseChar(X) when ?in(X, 16#01F4, 16#01F5) -> true; 
is_BaseChar(X) when ?in(X, 16#01FA, 16#0217) -> true; 
is_BaseChar(X) when ?in(X, 16#0250, 16#02A8) -> true; 
is_BaseChar(X) when ?in(X, 16#02BB, 16#02C1) -> true; 
is_BaseChar(16#0386) -> true;
is_BaseChar(X) when ?in(X, 16#0388, 16#038A) -> true; 
is_BaseChar(16#038C) -> true;
is_BaseChar(X) when ?in(X, 16#038E, 16#03A1) -> true; 
is_BaseChar(X) when ?in(X, 16#03A3, 16#03CE) -> true; 
is_BaseChar(X) when ?in(X, 16#03D0, 16#03D6) -> true; 
is_BaseChar(16#03DA) -> true;
is_BaseChar(16#03DC) -> true;
is_BaseChar(16#03DE) -> true;
is_BaseChar(16#03E0) -> true;
is_BaseChar(X) when ?in(X, 16#03E2, 16#03F3) -> true; 
is_BaseChar(X) when ?in(X, 16#0401, 16#040C) -> true; 
is_BaseChar(X) when ?in(X, 16#040E, 16#044F) -> true; 
is_BaseChar(X) when ?in(X, 16#0451, 16#045C) -> true; 
is_BaseChar(X) when ?in(X, 16#045E, 16#0481) -> true; 
is_BaseChar(X) when ?in(X, 16#0490, 16#04C4) -> true; 
is_BaseChar(X) when ?in(X, 16#04C7, 16#04C8) -> true; 
is_BaseChar(X) when ?in(X, 16#04CB, 16#04CC) -> true; 
is_BaseChar(X) when ?in(X, 16#04D0, 16#04EB) -> true; 
is_BaseChar(X) when ?in(X, 16#04EE, 16#04F5) -> true; 
is_BaseChar(X) when ?in(X, 16#04F8, 16#04F9) -> true; 
is_BaseChar(X) when ?in(X, 16#0531, 16#0556) -> true; 
is_BaseChar(16#0559) -> true;
is_BaseChar(X) when ?in(X, 16#0561, 16#0586) -> true; 
is_BaseChar(X) when ?in(X, 16#05D0, 16#05EA) -> true; 
is_BaseChar(X) when ?in(X, 16#05F0, 16#05F2) -> true; 
is_BaseChar(X) when ?in(X, 16#0621, 16#063A) -> true; 
is_BaseChar(X) when ?in(X, 16#0641, 16#064A) -> true; 
is_BaseChar(X) when ?in(X, 16#0671, 16#06B7) -> true; 
is_BaseChar(X) when ?in(X, 16#06BA, 16#06BE) -> true; 
is_BaseChar(X) when ?in(X, 16#06C0, 16#06CE) -> true; 
is_BaseChar(X) when ?in(X, 16#06D0, 16#06D3) -> true; 
is_BaseChar(16#06D5) -> true;
is_BaseChar(X) when ?in(X, 16#06E5, 16#06E6) -> true; 
is_BaseChar(X) when ?in(X, 16#0905, 16#0939) -> true; 
is_BaseChar(16#093D) -> true;
is_BaseChar(X) when ?in(X, 16#0958, 16#0961) -> true; 
is_BaseChar(X) when ?in(X, 16#0985, 16#098C) -> true; 
is_BaseChar(X) when ?in(X, 16#098F, 16#0990) -> true; 
is_BaseChar(X) when ?in(X, 16#0993, 16#09A8) -> true; 
is_BaseChar(X) when ?in(X, 16#09AA, 16#09B0) -> true; 
is_BaseChar(16#09B2) -> true;
is_BaseChar(X) when ?in(X, 16#09B6, 16#09B9) -> true; 
is_BaseChar(X) when ?in(X, 16#09DC, 16#09DD) -> true; 
is_BaseChar(X) when ?in(X, 16#09DF, 16#09E1) -> true; 
is_BaseChar(X) when ?in(X, 16#09F0, 16#09F1) -> true; 
is_BaseChar(X) when ?in(X, 16#0A05, 16#0A0A) -> true; 
is_BaseChar(X) when ?in(X, 16#0A0F, 16#0A10) -> true; 
is_BaseChar(X) when ?in(X, 16#0A13, 16#0A28) -> true; 
is_BaseChar(X) when ?in(X, 16#0A2A, 16#0A30) -> true; 
is_BaseChar(X) when ?in(X, 16#0A32, 16#0A33) -> true; 
is_BaseChar(X) when ?in(X, 16#0A35, 16#0A36) -> true; 
is_BaseChar(X) when ?in(X, 16#0A38, 16#0A39) -> true; 
is_BaseChar(X) when ?in(X, 16#0A59, 16#0A5C) -> true; 
is_BaseChar(16#0A5E) -> true;
is_BaseChar(X) when ?in(X, 16#0A72, 16#0A74) -> true; 
is_BaseChar(X) when ?in(X, 16#0A85, 16#0A8B) -> true; 
is_BaseChar(16#0A8D) -> true;
is_BaseChar(X) when ?in(X, 16#0A8F, 16#0A91) -> true; 
is_BaseChar(X) when ?in(X, 16#0A93, 16#0AA8) -> true; 
is_BaseChar(X) when ?in(X, 16#0AAA, 16#0AB0) -> true; 
is_BaseChar(X) when ?in(X, 16#0AB2, 16#0AB3) -> true; 
is_BaseChar(X) when ?in(X, 16#0AB5, 16#0AB9) -> true; 
is_BaseChar(16#0ABD) -> true;
is_BaseChar(16#0AE0) -> true;
is_BaseChar(X) when ?in(X, 16#0B05, 16#0B0C) -> true; 
is_BaseChar(X) when ?in(X, 16#0B0F, 16#0B10) -> true; 
is_BaseChar(X) when ?in(X, 16#0B13, 16#0B28) -> true; 
is_BaseChar(X) when ?in(X, 16#0B2A, 16#0B30) -> true; 
is_BaseChar(X) when ?in(X, 16#0B32, 16#0B33) -> true; 
is_BaseChar(X) when ?in(X, 16#0B36, 16#0B39) -> true; 
is_BaseChar(16#0B3D) -> true;
is_BaseChar(X) when ?in(X, 16#0B5C, 16#0B5D) -> true; 
is_BaseChar(X) when ?in(X, 16#0B5F, 16#0B61) -> true; 
is_BaseChar(X) when ?in(X, 16#0B85, 16#0B8A) -> true; 
is_BaseChar(X) when ?in(X, 16#0B8E, 16#0B90) -> true; 
is_BaseChar(X) when ?in(X, 16#0B92, 16#0B95) -> true; 
is_BaseChar(X) when ?in(X, 16#0B99, 16#0B9A) -> true; 
is_BaseChar(16#0B9C) -> true;
is_BaseChar(X) when ?in(X, 16#0B9E, 16#0B9F) -> true; 
is_BaseChar(X) when ?in(X, 16#0BA3, 16#0BA4) -> true; 
is_BaseChar(X) when ?in(X, 16#0BA8, 16#0BAA) -> true; 
is_BaseChar(X) when ?in(X, 16#0BAE, 16#0BB5) -> true; 
is_BaseChar(X) when ?in(X, 16#0BB7, 16#0BB9) -> true; 
is_BaseChar(X) when ?in(X, 16#0C05, 16#0C0C) -> true; 
is_BaseChar(X) when ?in(X, 16#0C0E, 16#0C10) -> true; 
is_BaseChar(X) when ?in(X, 16#0C12, 16#0C28) -> true; 
is_BaseChar(X) when ?in(X, 16#0C2A, 16#0C33) -> true; 
is_BaseChar(X) when ?in(X, 16#0C35, 16#0C39) -> true; 
is_BaseChar(X) when ?in(X, 16#0C60, 16#0C61) -> true; 
is_BaseChar(X) when ?in(X, 16#0C85, 16#0C8C) -> true; 
is_BaseChar(X) when ?in(X, 16#0C8E, 16#0C90) -> true; 
is_BaseChar(X) when ?in(X, 16#0C92, 16#0CA8) -> true; 
is_BaseChar(X) when ?in(X, 16#0CAA, 16#0CB3) -> true; 
is_BaseChar(X) when ?in(X, 16#0CB5, 16#0CB9) -> true; 
is_BaseChar(16#0CDE) -> true;
is_BaseChar(X) when ?in(X, 16#0CE0, 16#0CE1) -> true; 
is_BaseChar(X) when ?in(X, 16#0D05, 16#0D0C) -> true; 
is_BaseChar(X) when ?in(X, 16#0D0E, 16#0D10) -> true; 
is_BaseChar(X) when ?in(X, 16#0D12, 16#0D28) -> true; 
is_BaseChar(X) when ?in(X, 16#0D2A, 16#0D39) -> true; 
is_BaseChar(X) when ?in(X, 16#0D60, 16#0D61) -> true; 
is_BaseChar(X) when ?in(X, 16#0E01, 16#0E2E) -> true; 
is_BaseChar(16#0E30) -> true;
is_BaseChar(X) when ?in(X, 16#0E32, 16#0E33) -> true; 
is_BaseChar(X) when ?in(X, 16#0E40, 16#0E45) -> true; 
is_BaseChar(X) when ?in(X, 16#0E81, 16#0E82) -> true; 
is_BaseChar(16#0E84) -> true;
is_BaseChar(X) when ?in(X, 16#0E87, 16#0E88) -> true; 
is_BaseChar(16#0E8A) -> true;
is_BaseChar(16#0E8D) -> true;
is_BaseChar(X) when ?in(X, 16#0E94, 16#0E97) -> true; 
is_BaseChar(X) when ?in(X, 16#0E99, 16#0E9F) -> true; 
is_BaseChar(X) when ?in(X, 16#0EA1, 16#0EA3) -> true; 
is_BaseChar(16#0EA5) -> true;
is_BaseChar(16#0EA7) -> true;
is_BaseChar(X) when ?in(X, 16#0EAA, 16#0EAB) -> true; 
is_BaseChar(X) when ?in(X, 16#0EAD, 16#0EAE) -> true; 
is_BaseChar(16#0EB0) -> true;
is_BaseChar(X) when ?in(X, 16#0EB2, 16#0EB3) -> true; 
is_BaseChar(16#0EBD) -> true;
is_BaseChar(X) when ?in(X, 16#0EC0, 16#0EC4) -> true; 
is_BaseChar(X) when ?in(X, 16#0F40, 16#0F47) -> true; 
is_BaseChar(X) when ?in(X, 16#0F49, 16#0F69) -> true; 
is_BaseChar(X) when ?in(X, 16#10A0, 16#10C5) -> true; 
is_BaseChar(X) when ?in(X, 16#10D0, 16#10F6) -> true; 
is_BaseChar(16#1100) -> true;
is_BaseChar(X) when ?in(X, 16#1102, 16#1103) -> true; 
is_BaseChar(X) when ?in(X, 16#1105, 16#1107) -> true; 
is_BaseChar(16#1109) -> true;
is_BaseChar(X) when ?in(X, 16#110B, 16#110C) -> true; 
is_BaseChar(X) when ?in(X, 16#110E, 16#1112) -> true; 
is_BaseChar(16#113C) -> true;
is_BaseChar(16#113E) -> true;
is_BaseChar(16#1140) -> true;
is_BaseChar(16#114C) -> true;
is_BaseChar(16#114E) -> true;
is_BaseChar(16#1150) -> true;
is_BaseChar(X) when ?in(X, 16#1154, 16#1155) -> true; 
is_BaseChar(16#1159) -> true;
is_BaseChar(X) when ?in(X, 16#115F, 16#1161) -> true; 
is_BaseChar(16#1163) -> true;
is_BaseChar(16#1165) -> true;
is_BaseChar(16#1167) -> true;
is_BaseChar(16#1169) -> true;
is_BaseChar(X) when ?in(X, 16#116D, 16#116E) -> true; 
is_BaseChar(X) when ?in(X, 16#1172, 16#1173) -> true; 
is_BaseChar(16#1175) -> true;
is_BaseChar(16#119E) -> true;
is_BaseChar(16#11A8) -> true;
is_BaseChar(16#11AB) -> true;
is_BaseChar(X) when ?in(X, 16#11AE, 16#11AF) -> true; 
is_BaseChar(X) when ?in(X, 16#11B7, 16#11B8) -> true; 
is_BaseChar(16#11BA) -> true;
is_BaseChar(X) when ?in(X, 16#11BC, 16#11C2) -> true; 
is_BaseChar(16#11EB) -> true;
is_BaseChar(16#11F0) -> true;
is_BaseChar(16#11F9) -> true;
is_BaseChar(X) when ?in(X, 16#1E00, 16#1E9B) -> true; 
is_BaseChar(X) when ?in(X, 16#1EA0, 16#1EF9) -> true; 
is_BaseChar(X) when ?in(X, 16#1F00, 16#1F15) -> true; 
is_BaseChar(X) when ?in(X, 16#1F18, 16#1F1D) -> true; 
is_BaseChar(X) when ?in(X, 16#1F20, 16#1F45) -> true; 
is_BaseChar(X) when ?in(X, 16#1F48, 16#1F4D) -> true; 
is_BaseChar(X) when ?in(X, 16#1F50, 16#1F57) -> true; 
is_BaseChar(16#1F59) -> true;
is_BaseChar(16#1F5B) -> true;
is_BaseChar(16#1F5D) -> true;
is_BaseChar(X) when ?in(X, 16#1F5F, 16#1F7D) -> true; 
is_BaseChar(X) when ?in(X, 16#1F80, 16#1FB4) -> true; 
is_BaseChar(X) when ?in(X, 16#1FB6, 16#1FBC) -> true; 
is_BaseChar(16#1FBE) -> true;
is_BaseChar(X) when ?in(X, 16#1FC2, 16#1FC4) -> true; 
is_BaseChar(X) when ?in(X, 16#1FC6, 16#1FCC) -> true; 
is_BaseChar(X) when ?in(X, 16#1FD0, 16#1FD3) -> true; 
is_BaseChar(X) when ?in(X, 16#1FD6, 16#1FDB) -> true; 
is_BaseChar(X) when ?in(X, 16#1FE0, 16#1FEC) -> true; 
is_BaseChar(X) when ?in(X, 16#1FF2, 16#1FF4) -> true; 
is_BaseChar(X) when ?in(X, 16#1FF6, 16#1FFC) -> true; 
is_BaseChar(16#2126) -> true;
is_BaseChar(X) when ?in(X, 16#212A, 16#212B) -> true; 
is_BaseChar(16#212E) -> true;
is_BaseChar(X) when ?in(X, 16#2180, 16#2182) -> true; 
is_BaseChar(X) when ?in(X, 16#3041, 16#3094) -> true; 
is_BaseChar(X) when ?in(X, 16#30A1, 16#30FA) -> true; 
is_BaseChar(X) when ?in(X, 16#3105, 16#312C) -> true; 
is_BaseChar(X) when ?in(X, 16#AC00, 16#D7A3) -> true;
is_BaseChar(_) -> false.

%% [86] Ideographic	   ::= ...
   	
is_Ideographic(X) when ?in(X, 16#4E00, 16#9FA5) -> true; 
is_Ideographic(16#3007) -> true;
is_Ideographic(X) when ?in(X, 16#3021, 16#3029) -> true;
is_Ideographic(_) -> false.

%% [87] CombiningChar	   ::= ...

is_CombiningChar(X) when ?in(X, 16#0300, 16#0345) -> true; 
is_CombiningChar(X) when ?in(X, 16#0360, 16#0361) -> true; 
is_CombiningChar(X) when ?in(X, 16#0483, 16#0486) -> true; 
is_CombiningChar(X) when ?in(X, 16#0591, 16#05A1) -> true; 
is_CombiningChar(X) when ?in(X, 16#05A3, 16#05B9) -> true; 
is_CombiningChar(X) when ?in(X, 16#05BB, 16#05BD) -> true; 
is_CombiningChar(16#05BF) -> true;
is_CombiningChar(X) when ?in(X, 16#05C1, 16#05C2) -> true; 
is_CombiningChar(16#05C4) -> true;
is_CombiningChar(X) when ?in(X, 16#064B, 16#0652) -> true; 
is_CombiningChar(16#0670) -> true;
is_CombiningChar(X) when ?in(X, 16#06D6, 16#06DC) -> true; 
is_CombiningChar(X) when ?in(X, 16#06DD, 16#06DF) -> true; 
is_CombiningChar(X) when ?in(X, 16#06E0, 16#06E4) -> true; 
is_CombiningChar(X) when ?in(X, 16#06E7, 16#06E8) -> true; 
is_CombiningChar(X) when ?in(X, 16#06EA, 16#06ED) -> true; 
is_CombiningChar(X) when ?in(X, 16#0901, 16#0903) -> true; 
is_CombiningChar(16#093C) -> true;
is_CombiningChar(X) when ?in(X, 16#093E, 16#094C) -> true; 
is_CombiningChar(16#094D) -> true;
is_CombiningChar(X) when ?in(X, 16#0951, 16#0954) -> true; 
is_CombiningChar(X) when ?in(X, 16#0962, 16#0963) -> true; 
is_CombiningChar(X) when ?in(X, 16#0981, 16#0983) -> true; 
is_CombiningChar(16#09BC) -> true;
is_CombiningChar(16#09BE) -> true;
is_CombiningChar(16#09BF) -> true;
is_CombiningChar(X) when ?in(X, 16#09C0, 16#09C4) -> true; 
is_CombiningChar(X) when ?in(X, 16#09C7, 16#09C8) -> true; 
is_CombiningChar(X) when ?in(X, 16#09CB, 16#09CD) -> true; 
is_CombiningChar(16#09D7) -> true;
is_CombiningChar(X) when ?in(X, 16#09E2, 16#09E3) -> true; 
is_CombiningChar(16#0A02) -> true;
is_CombiningChar(16#0A3C) -> true;
is_CombiningChar(16#0A3E) -> true;
is_CombiningChar(16#0A3F) -> true;
is_CombiningChar(X) when ?in(X, 16#0A40, 16#0A42) -> true; 
is_CombiningChar(X) when ?in(X, 16#0A47, 16#0A48) -> true; 
is_CombiningChar(X) when ?in(X, 16#0A4B, 16#0A4D) -> true; 
is_CombiningChar(X) when ?in(X, 16#0A70, 16#0A71) -> true; 
is_CombiningChar(X) when ?in(X, 16#0A81, 16#0A83) -> true; 
is_CombiningChar(16#0ABC) -> true;
is_CombiningChar(X) when ?in(X, 16#0ABE, 16#0AC5) -> true; 
is_CombiningChar(X) when ?in(X, 16#0AC7, 16#0AC9) -> true; 
is_CombiningChar(X) when ?in(X, 16#0ACB, 16#0ACD) -> true; 
is_CombiningChar(X) when ?in(X, 16#0B01, 16#0B03) -> true; 
is_CombiningChar(16#0B3C) -> true;
is_CombiningChar(X) when ?in(X, 16#0B3E, 16#0B43) -> true; 
is_CombiningChar(X) when ?in(X, 16#0B47, 16#0B48) -> true; 
is_CombiningChar(X) when ?in(X, 16#0B4B, 16#0B4D) -> true; 
is_CombiningChar(X) when ?in(X, 16#0B56, 16#0B57) -> true; 
is_CombiningChar(X) when ?in(X, 16#0B82, 16#0B83) -> true; 
is_CombiningChar(X) when ?in(X, 16#0BBE, 16#0BC2) -> true; 
is_CombiningChar(X) when ?in(X, 16#0BC6, 16#0BC8) -> true; 
is_CombiningChar(X) when ?in(X, 16#0BCA, 16#0BCD) -> true; 
is_CombiningChar(16#0BD7) -> true;
is_CombiningChar(X) when ?in(X, 16#0C01, 16#0C03) -> true; 
is_CombiningChar(X) when ?in(X, 16#0C3E, 16#0C44) -> true; 
is_CombiningChar(X) when ?in(X, 16#0C46, 16#0C48) -> true; 
is_CombiningChar(X) when ?in(X, 16#0C4A, 16#0C4D) -> true; 
is_CombiningChar(X) when ?in(X, 16#0C55, 16#0C56) -> true; 
is_CombiningChar(X) when ?in(X, 16#0C82, 16#0C83) -> true; 
is_CombiningChar(X) when ?in(X, 16#0CBE, 16#0CC4) -> true; 
is_CombiningChar(X) when ?in(X, 16#0CC6, 16#0CC8) -> true; 
is_CombiningChar(X) when ?in(X, 16#0CCA, 16#0CCD) -> true; 
is_CombiningChar(X) when ?in(X, 16#0CD5, 16#0CD6) -> true; 
is_CombiningChar(X) when ?in(X, 16#0D02, 16#0D03) -> true; 
is_CombiningChar(X) when ?in(X, 16#0D3E, 16#0D43) -> true; 
is_CombiningChar(X) when ?in(X, 16#0D46, 16#0D48) -> true; 
is_CombiningChar(X) when ?in(X, 16#0D4A, 16#0D4D) -> true; 
is_CombiningChar(16#0D57) -> true;
is_CombiningChar(16#0E31) -> true;
is_CombiningChar(X) when ?in(X, 16#0E34, 16#0E3A) -> true; 
is_CombiningChar(X) when ?in(X, 16#0E47, 16#0E4E) -> true; 
is_CombiningChar(16#0EB1) -> true;
is_CombiningChar(X) when ?in(X, 16#0EB4, 16#0EB9) -> true; 
is_CombiningChar(X) when ?in(X, 16#0EBB, 16#0EBC) -> true; 
is_CombiningChar(X) when ?in(X, 16#0EC8, 16#0ECD) -> true; 
is_CombiningChar(X) when ?in(X, 16#0F18, 16#0F19) -> true; 
is_CombiningChar(16#0F35) -> true;
is_CombiningChar(16#0F37) -> true;
is_CombiningChar(16#0F39) -> true;
is_CombiningChar(16#0F3E) -> true;
is_CombiningChar(16#0F3F) -> true;
is_CombiningChar(X) when ?in(X, 16#0F71, 16#0F84) -> true; 
is_CombiningChar(X) when ?in(X, 16#0F86, 16#0F8B) -> true; 
is_CombiningChar(X) when ?in(X, 16#0F90, 16#0F95) -> true; 
is_CombiningChar(16#0F97) -> true;
is_CombiningChar(X) when ?in(X, 16#0F99, 16#0FAD) -> true; 
is_CombiningChar(X) when ?in(X, 16#0FB1, 16#0FB7) -> true; 
is_CombiningChar(16#0FB9) -> true;
is_CombiningChar(X) when ?in(X, 16#20D0, 16#20DC) -> true; 
is_CombiningChar(16#20E1) -> true;
is_CombiningChar(X) when ?in(X, 16#302A, 16#302F) -> true; 
is_CombiningChar(16#3099) -> true;
is_CombiningChar(16#309A) -> true;
is_CombiningChar(_) -> false.

%% Digit	   ::=  
is_Digit(X) when ?in(X, 16#0030, 16#0039) -> true; 
is_Digit(X) when ?in(X, 16#0660, 16#0669) -> true; 
is_Digit(X) when ?in(X, 16#06F0, 16#06F9) -> true; 
is_Digit(X) when ?in(X, 16#0966, 16#096F) -> true; 
is_Digit(X) when ?in(X, 16#09E6, 16#09EF) -> true; 
is_Digit(X) when ?in(X, 16#0A66, 16#0A6F) -> true; 
is_Digit(X) when ?in(X, 16#0AE6, 16#0AEF) -> true; 
is_Digit(X) when ?in(X, 16#0B66, 16#0B6F) -> true; 
is_Digit(X) when ?in(X, 16#0BE7, 16#0BEF) -> true; 
is_Digit(X) when ?in(X, 16#0C66, 16#0C6F) -> true; 
is_Digit(X) when ?in(X, 16#0CE6, 16#0CEF) -> true; 
is_Digit(X) when ?in(X, 16#0D66, 16#0D6F) -> true; 
is_Digit(X) when ?in(X, 16#0E50, 16#0E59) -> true; 
is_Digit(X) when ?in(X, 16#0ED0, 16#0ED9) -> true; 
is_Digit(X) when ?in(X, 16#0F20, 16#0F29) -> true;
is_Digit(_) -> false.

%% [89]	Extender	   ::= ...

is_Extender(16#00B7) -> true;
is_Extender(16#02D0) -> true;
is_Extender(16#02D1) -> true;
is_Extender(16#0387) -> true;
is_Extender(16#0640) -> true;
is_Extender(16#0E46) -> true;
is_Extender(16#0EC6) -> true;
is_Extender(16#3005) -> true;
is_Extender(X) when ?in(X, 16#3031, 16#3035) -> true; 
is_Extender(X) when ?in(X, 16#309D, 16#309E) -> true; 
is_Extender(X) when ?in(X, 16#30FC, 16#30FE) -> true; 
is_Extender(_) -> false.

%% test_tokenize()
%%   take a file and tokenizes it using 100 byte chunks
%%   Then split the file into smaller and smaller chunks and
%%   tokenizes these chunks and checks that the result is the same as in
%%   the origonal tokenization

test_tokenize() ->
    {ok, Bin} = file:read_file("../ezxml-2.0/doc.chap"),
    L = binary_to_list(Bin),
    %% L = "<abc>def</a><a/><b a='123'>aaa",
    %% F2 = fun(Tok, State) -> io:format("~p~n",[Tok]),[Tok|State] end,
    F2 = fun(Tok, State) -> [Tok|State] end,
    F1 = fun() -> get_data(100, L) end,
    Toks = tokenize(F1, F2, []),
    test(100, L, F2, Toks).

test(0, _, _, _) ->
    horray;
test(N, L, F2, Toks) ->
    io:format("size:~p~n",[N]),
    F1 = fun() -> get_data(N, L) end,
    Toks1 = tokenize(F1, F2, []),
    case Toks1 of
	Toks ->
	    test(N-1, L, F2, Toks);
	_ ->
	    lib_misc:dump("debug", {toks, Toks, toks1, Toks1}), 
	    {error, N} 
    end.

get_data(_, []) -> 
    eos;
get_data(N, L) -> 
    {A, B} = take(N, L),
    F1  =fun() -> get_data(N, B) end,    
    %% io:format("A=~p (next)=~p ~n",[A, F1()]),
    {A, F1}.

take(N, L) ->			   
    take(N, L, []).

take(_, [], L) ->
    {reverse(L), []};
take(0, T, L) ->
    {reverse(L), T};
take(N, [H|T], L) ->
    take(N-1, T, [H|L]).

%% parse takes a Token production function
%% F1() = {Tok, F1'} where Tok = Tok | eof

parse(F) -> parse(F, []).

parse(F1, Errs) ->
    case F1() of
	{{sTag, Ln, Tag, Args}, F2} ->
	    {Body, F3, Errs1} = get_body(F2, Ln, Tag, [], Errs),
	    {{node, Tag, Ln, Args,Body}, F3, Errs1};
	{Val, F2}  ->
	    {Val, F2, Errs}
    end.

get_body(F1, Ln, Tag, L, Errs) ->
    {Token, F2} = F1(),
    case Token of
	{eTag, _, Tag} ->
	    {reverse(L), F2, Errs};
	{eTag, Ln1, OtherTag} ->
	    io:format("** dropping bad tag ~p found in line:~p~n"
		      "start tag in line:~p was ~p~n",
		      [OtherTag, Ln1, Ln, Tag]),
	    get_body(F2, Ln, Tag, L, 
		     [{badTag,OtherTag,inLine,Ln1,startTag,Tag,inLine,Ln}|Errs]);
	eof ->
	    io:format("** unexpected eof~n"
		      "start tag ~p in line:~p is unclosed~n",
		      [Tag, Ln]),
	    exit(eBadXML);
	{sTag, Ln1, Tag1, Args} ->
	    {Body, F3, Errs1} = get_body(F2, Ln1, Tag1, [], Errs),
	    get_body(F3, Ln, Tag, [{node,Tag1,Ln1,Args,Body}|L], Errs1);
	{raw,_,_,_} ->
	    get_body(F2, Ln, Tag, [Token|L], Errs);
	{empty, Ln1, Tag1, Args} ->
	    get_body(F2, Ln, Tag, [{node,Tag1,Ln1,Args,[]}|L], Errs);
	{comment,_,_} ->
	    get_body(F2, Ln, Tag, L, Errs);
	Other ->
	    io:format("** dropping unexpected***:~p~n",[Other]),
	    get_body(F2, Ln, Tag, L, [{unexpectedObject,Other}|Errs])
    end.

test_parse() -> parse_file("./lorem.chap").

parse_file(File) ->
    F1 = elib1_misc:file2stream(File),
    parse_stream(F1).

parse_string(Str) ->
    F1 = elib1_misc:string2stream(Str),
    parse_stream(F1).

parse_stream(F1) ->
    F2 = fun(Tok, State) -> [Tok|State] end,
    Toks = reverse(tokenize(F1, F2, [])),
    %% lib_misc:dump("debug", {toks, Toks}), 
    GetNextTok = fun() -> get_tok(Toks) end,
    Result = parse_loop(GetNextTok, [], []),
    %% lib_misc:dump("parsed", {tree, Tree}),
    Result.

parse_loop(F, L, Errs) ->
    {Val, F1, Errs1} = parse(F, Errs),
    case Val of 
	eof -> {reverse(L), reverse(Errs1)};
	_   -> parse_loop(F1, [Val|L], Errs1)
    end.

get_tok([H|T]) -> {H,   fun() -> get_tok(T)  end};
get_tok([])    -> {eof, fun() -> get_tok([]) end}.

check_grammar(Line, Tag, Args, Body) ->
    io:format("Check_grammar:Ln:~p Tag=~p Args=~p Body=~p~n",
	      [Line,Tag,Args,Body]).
