%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(irc_expt).

-compile(export_all).
-import(lists, [member/2]).

test() ->
    %% p1, p2, p3, p4 are pids
    Gs = [{<<"#mnesia">>,    [<<"mnesia-master">>,<<"joe">>,<<"jim">>]},
	  {<<"#webserver">>, [<<"webserver-master">>, <<"jim">>]},
	  {<<"erlang">>,     [<<"jim">>,<<"fred">>]},
	  {<<"chat">>,       [<<"jim">>,<<"mary">>]}],
    Ps = [{<<"jim">>,p1},
	  {<<"joe">>,p2},
	  {<<"fred">>,p3},
	  {<<"mary">>, p4},
	  {<<"mnesia-master">>, p5},
	  {<<"webserver-master">>, p6}],
    %% add_user
    {error, eDuplicateUser} = add_user(<<"fred">>, p7, Ps),
    [{<<"sue">>,p8}|Ps] = add_user(<<"sue">>, p8, Ps),
    %% try to add joe to mnesia - this is false because joe
    %% is in mnesia
    false = add_user_to_group(<<"joe">>, <<"#mnesia">>, Gs),
    %% add mary to mnesia this time it succeeds
    {value,[{<<"#mnesia">>,
	     [<<"mary">>,<<"mnesia-master">>,<<"joe">>,<<"jim">>]},
	    {<<"#webserver">>,[<<"webserver-master">>,<<"jim">>]},
	    {<<"erlang">>,[<<"jim">>,<<"fred">>]},
	    {<<"chat">>,[<<"jim">>,<<"mary">>]}]} =
	add_user_to_group(<<"mary">>, <<"#mnesia">>, Gs),
    ok,
    %% delete_user_from_group
    %% sue is not in #mnesia
    false = delete_user_from_group(<<"sue">>, <<"#mnesia">>, Gs),
    %%
    false = delete_user_from_group(<<"sue">>, <<"#nosuchgroup">>, Gs),
    {value,Ga = [{<<"#mnesia">>,
		  [<<"mnesia-master">>,<<"joe">>,<<"jim">>]},
		 {<<"#webserver">>,[<<"webserver-master">>,<<"jim">>]},
		 {<<"erlang">>,[<<"fred">>]},
		 {<<"chat">>,[<<"jim">>,<<"mary">>]}]} =
	delete_user_from_group(<<"jim">>, <<"erlang">>, Gs),
    {value,[{<<"#mnesia">>,
	     [<<"mnesia-master">>,<<"joe">>,<<"jim">>]},
	    {<<"#webserver">>,[<<"webserver-master">>,<<"jim">>]},
	    {<<"chat">>,[<<"jim">>,<<"mary">>]}]} =
	delete_user_from_group(<<"fred">>, <<"erlang">>, Ga),
    [p5,p2,p1] = pids(<<"#mnesia">>, Ps, Gs),
    [p2] = pid(<<"joe">>, Ps).

pids(Group, Ps, Gs) ->
    [Pid || {G,L} <- Gs, G =:= Group,
	    Nick <- L,
	    {N, Pid} <- Ps,
	    Nick =:= N].

pid(Nick, Ps) ->
    [Pid || {N, Pid} <- Ps, Nick =:= N].

add_user_to_group(Who, Group, Gs) ->
    case lists:keysearch(Group, 1, Gs) of
	{value, {_, L}} ->
	    case member(Who, L) of
		true  ->
		    false;
		false ->
		    {value, lists:keyreplace(Group, 1, Gs, {Group,[Who|L]})}
	    end;
	false ->
	    {value, [{Group, [Who]}|Gs]}
    end.

delete_user_from_group(Who, Group, Gs) ->
    case lists:keysearch(Group, 1, Gs) of
	{value, {_, L}} ->
	    case member(Who, L) of
		true  ->
		    L1 = lists:delete(Who, L),
		    case L1 of
			[] ->
			    {value, lists:keydelete(Group, 1, Gs)};
			_ ->
			    {value, lists:keyreplace(Group, 1, Gs, {Group, L1})}
		    end;
		false ->
		    false
	    end;
	false ->
	    false
    end.

add_user(U, Pid, Ps) ->
    case lists:keysearch(U, 1, Ps) of
	false -> [{U,Pid}|Ps];
	_     -> {error, eDuplicateUser}
    end.

delete_user(U, Ps, Gs) ->
    %% -> Ps', Gs', Ginform
    %% return a new list of Ps and Gs and a list of groups to inform
    a.
