%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.
%% Time-stamp: <2009-12-09 17:40:44 joe>

-module(elib1_expand).

%% @doc Expand a file of string conting inline Erlang expressions
%% For example:
%% <pre>
%% &lt;? X=123,Y=2,"Hello"?> Joe X is &lt;? erlang:integer_to_list(X) ?>
%% </pre>
%% Expands into
%% <pre>
%% Hello Joe X is 123
%% </pre>

-export([batch/1, insert/1, insert/2, expand_string/1, expand_string/2,
	 expand_file/1,
	 quote_file/1]).

-import(lists, [reverse/1, reverse/2]).
-include_lib("eunit/include/eunit.hrl").

string_test() ->
    Str = "<? X=123,Y=2,\"Hello\"?> Joe, X is <? erlang:integer_to_list(X) ?>",
    "Hello Joe, X is 123" = expand_string(Str).

%% This is called from the makefile


batch([In,Out]) ->
    convert(atom_to_list(In), atom_to_list(Out)),
    init:stop().

expand_file(File) ->
    {ok, B} = file:read_file(File),
    expand_string(binary_to_list(B)).

convert(In, Out) ->
    {ok, B} = file:read_file(In),
    L = expand_string(binary_to_list(B)),
    file:write_file(Out, L),
    io:format("elib1_expand:Created ~s~n",[Out]).

%% START:tag1
expand_string(Str) ->
    expand_string(Str, erl_eval:new_bindings()).

expand_string(Str, Bs) ->
    expand(Str, [], Bs).

expand("<?" ++ T, L, Bindings0) ->
    {Str, T1} = collect_erlang(T, []),
    %% io:format("DO::~s::~n",[Str]),
    {Value, Bindings1} = string2value(Str, Bindings0),
    expand(T1, reverse(Value, L), Bindings1);
expand([H|T], L, B) ->
    expand(T, [H|L], B);
expand([], L, _) ->
    reverse(L).
%% END:tag1

collect_erlang("?>" ++ T, L) -> {reverse(L),T};
collect_erlang([H|T], L)     -> collect_erlang(T, [H|L]).

string2value(Str, Bindings0) ->
    {ok, Tokens, _} = erl_scan:string(Str ++ "."),
    {ok, Exprs} = erl_parse:parse_exprs(Tokens),
    Exprs1 = add_default_module(Exprs),
    {value, Value, Bindings1} = erl_eval:exprs(Exprs1, Bindings0),
    {Value, Bindings1}.

%% insert an entire erlang file

insert(F) ->
    {ok, B} = file:read_file(F),
    Str = elib1_misc:expand_tabs(binary_to_list(B)),
    ["<code>\n",string2html(Str),"</code>\n"].

string2html("<" ++ T) -> "&lt;"  ++ string2html(T);
string2html("&" ++ T) -> "&amp;" ++ string2html(T);
string2html([H|T])    -> [H|string2html(T)];            
string2html([])       -> [].

%% this adds this module to be the default if no module is supplied

add_default_module({call,Ln,{atom,_,_}=Func, Args}) ->
    {call,Ln,
     {remote,Ln,{atom,Ln,?MODULE},Func}, 
     add_default_module(Args)};
add_default_module(T) when is_tuple(T) ->
    list_to_tuple(add_default_module(tuple_to_list(T)));
add_default_module([H|T]) ->
    [add_default_module(H)|add_default_module(T)];
add_default_module(X) ->
    X.


%%----------------------------------------------------------------------
%% insert(File, Tag) -> Data | exit + warning message
%%    Looks in File for a section marked marked like this:
%%    %% START:Tag
%%    ....
%%    %% END:Tag
%% This pattern *must* be exactly as shown - no extra blanks etc

%% START:test
insert(File, Tag) ->
    case file:read_file(File) of
	{ok, B} ->
	    Stuff = get_stuff(binary_to_list(B), File, Tag),
	    Stuff1 = elib1_misc:expand_tabs(Stuff),
	    ["<code>\n", string2html(Stuff1), "</code>\n"];
	{error, _} ->
	    Str = lists:flatten(io_lib:format(
				  "*** ERROR: Missing File: ~s~n",[File])),
	    exit({eInsert,File,Str})
    end.
%% END:test


get_stuff(Str, _, all) -> Str;
get_stuff(Str, File, Tag) -> get_stuff1(Str, File, Tag).

get_stuff1("%% START:" ++ T, File, Tag) ->
    case matches(T, Tag) of
	{yes, T1} -> get_content(T1, File, Tag, []);
	no        -> get_stuff1(T, File, Tag)
    end;
get_stuff1([_|T], File, Tag) ->
    get_stuff1(T, File, Tag);
get_stuff1([], File, Tag) ->
    throw({eGetErlSection, noStartTag, File, Tag}).

get_content("%% START:" ++ T, File, Tag, L) ->
    get_content(skip_to_nl(T), File, Tag, L);
get_content("%% END:" ++ T, File, Tag, L) ->
    case matches(T, Tag) of
	{yes, _} ->
	    reverse(L);
	no ->
	    get_content(skip_to_nl(T), File, Tag, L)
    end;
get_content([H|T], File, Tag, L) ->
    get_content(T, File, Tag, [H|L]);
get_content([], File, Tag, _) ->
    throw({eGetErlSection, eofInTag, File, Tag}).

matches(T, [])          -> {yes, skip_to_nl(T)};
matches([H|T1], [H|T2]) -> matches(T1, T2);
matches(_, _)           -> no.

skip_to_nl([$\n|T]) -> T;
skip_to_nl([])      -> [];
skip_to_nl([_|T])   -> skip_to_nl(T).

quote_file(File) ->
    case file:read_file(File) of
	{ok, Bin} ->
	    L = binary_to_list(Bin),
	    ["<pre>\n<b>\n", expand_lt(L), "</b>\n</pre>\n"];
	_ ->
	    io:format("Cannot read:~p~n",[File]),
	    ""
    end.

expand_lt([$<|T]) -> 
    "&lt;" ++ expand_lt(T);
expand_lt([H|T]) ->
    [H|expand_lt(T)];
expand_lt([]) ->
    [].
