%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(elib1_mysql).

%% File:         mysql2.erl
%% Description:  MySQL driver
%% Started:      4 Aug 2005
%% Time-stamp:   <2009-09-22 21:04:13 joe>
%% Notes
%% Origonal author:    Magnus Ahltorp
%% Origonal Copyright: (c) 2001-2004 Kungliga Tekniska Högskolan
%% Modifications:      Fredrik Thulin <ft@it.su.se>
%%                     Joe Armstrong  <erlang@gamil.com>
%%
%% Testing:
%%   run mysql2:test(1), ... etc.
%% Notes:
%%   1)
%%     You have to provide your own password module
%%     like this
%%       -module(password).
%%       -export([username/1, password/1]).
%%       username(mysql) -> "<User>".
%%       password(mysql) -> "<Password>".
%%
%%   2) You must me able to access mysql with the above username
%%      and password.
%%        Try the command:
%%           $ mysql -u <User> -p
%%           Enter password: <Password>
%%     If this is ok then your passwords etc are correctly setup

%%
%% -compile(export_all).

-export([start/2, start/3, start/4, cmd/2,
	 insert/2,
	 quote/1,
	 stop/1, test/1, debug/2]).

-import(lists, [reverse/1, seq/2, zipwith/3]).

-define(LONG_PASSWORD, 1).
-define(FOUND_ROWS, 2).
-define(LONG_FLAG, 4).
-define(PROTOCOL_41, 512).
-define(TRANSACTIONS, 8192).
-define(CONNECT_WITH_DB, 8).
-define(MAX_PACKET_SIZE, 1000000).
-define(SECURE_CONNECTION, 32768).
-define(MYSQL_QUERY_OP, 3).

test(1) ->
    {ok, Pid} = start("localhost", 3306,
		      password:username(mysql), password:password(mysql)),
    cmd(Pid, "show databases"),
    cmd(Pid, "use test"),
    cmd(Pid, "show tables"),
    debug(Pid, true),
    cmd(Pid, "select * from country limit 0,5"),
    stop(Pid);

test(2) ->
    {ok, Pid} = start("localhost", 3306,
		      password:username(mysql), password:password(mysql)),
    cmd(Pid, "use test"),
    cmd(Pid, "drop table if exists hash"),
    cmd(Pid, "create table if not exists hash ("
	     "k CHAR(32) not null primary key,"   %% don't call this key
             "val longblob not null)"),
    cmd(Pid, "show tables"),
    cmd(Pid, "insert into hash values ('abc','123')"),
    cmd(Pid, insert("hash", [test,seq(0,255)])),
    V1 = cmd(Pid, "select * from hash"),
    cmd(Pid, "select val from hash where k='test'"),
    cmd(Pid, "update hash set val='123' where k='test'"),
    V2 = cmd(Pid, "select * from hash"),
    stop(Pid),
    {V1,V2}.

insert(Table, Vals) when is_list(Table) ->
    ["insert into ", Table, " values ", value_list(Vals)].

value_list(L) ->
    [$(, value_list1(L), $)].

value_list1([H])   -> quote(H);
value_list1([H|T]) -> [quote(H),$,|value_list1(T)].

debug(Pid, Flag) ->
    Pid ! {debug, Flag}.

cmd(Pid, Q) ->
    rpc(Pid, {do_query, Q}).

rpc(Pid, Q) ->
    Pid ! {self(), Q},
    receive
	{Pid, Reply} ->
	    Reply
    end.

stop(Pid) ->
    Pid ! stop.

%%----------------------------------------------------------------------
%% main entry point

start(User, Pass) -> start("localhost", 3306, User, Pass).

start(Host, User, Pass) -> start(Host, 3306, User, Pass).


start(Host, Port, User, Pass) ->
    S = self(),
    Pid = spawn_link(fun() -> init(Host, Port, User, Pass, S) end),
    receive
	{Pid, Ret} ->
	    Ret
    end.

init(Host, Port, User, Pass, Parent) ->
    S = self(),
    Pid = spawn_link(fun() -> socket_driver(Host, Port, S) end),
    receive
	{Pid, ok} ->
	    case do_auth(Pid, User, Pass) of
		{ok, Vsn} ->
		    Parent ! {self(), {ok, self()}},
		    top_loop(Vsn, Pid, false);
		{error,_} = E ->
		    Parent ! {self(), E}
	    end;
	{Pid, Error} ->
	    Parent ! {self(), Error}
    end.

top_loop(Vsn, Driver, Debug) ->
    receive
	{debug, Flag} ->
	    top_loop(Vsn, Driver, Flag);
	stop ->
	    Driver ! stop;
	{From, {do_query, Q}} ->
	    Response = do_query(Driver, Vsn, Debug, Q),
	    From ! {self(), Response},
	    top_loop(Vsn, Driver, Debug);
	Any ->
	    io:format("top_loop unexpected message:~p~n",[Any]),
	    top_loop(Vsn, Driver, Debug)
    end.

%%----------------------------------------------------------------------
%% socket_driver(Host, Port, Parent)
%%    try to open a socket to <Host,Port>
%%    send Parent ! {self(), ok}           if this succeeds
%%                  {self(), {error, Why}} if this fails

socket_driver(Host, Port, Parent) ->
    case gen_tcp:connect(Host, Port, [binary, {packet, 0}]) of
	{ok, Sock} ->
	    Parent ! {self(), ok},
	    driver_loop(Sock, Parent, <<>>);
	{error, _} = E ->
	    Parent ! {self(), E}
    end.

driver_loop(Sock, Pid, Bin0) ->
    receive
	stop ->
	    gen_tcp:close(Sock);
	{tcp, Sock, Bin1} ->
	    Bin2 = list_to_binary([Bin0, Bin1]),
	    %% send data to parent if we have enough data
	    Bin3 = sendpacket(Pid, Bin2),
	    driver_loop(Sock, Pid, Bin3);
	{tcp_error, Sock, Reason} ->
	    io:format("Socket error:~p ~p~n", [Sock, Reason]),
	    exit(oopps);
	{tcp_closed, Sock} ->
	    Pid ! {self(), closed};
	{send, Packet, Seq} ->
	    %% io:format("tosql:~p~n",[Packet]),
	    Bin = <<(size(Packet)):24/little, Seq:8, Packet/binary>>,
	    gen_tcp:send(Sock, Bin),
	    driver_loop(Sock, Pid, Bin0);
	Other ->
	    io:format("uugh:~p~n",[Other]),
	    driver_loop(Sock, Pid, Bin0)
    end.

sendpacket(Pid, Bin) ->
    case Bin of
	<<Length:24/little, Num:8, D/binary>> ->
	    if
		Length =< size(D) ->
		    {Packet, Rest} = split_binary(D, Length),
		    %% io:format("from mysql:~p~n",[{mysql, Packet, Num}]),
		    Pid ! {self(), {mysql, Packet, Num}},
		    sendpacket(Pid, Rest);
		true ->
		    Bin
	    end;
	_ ->
	    Bin
    end.

%%----------------------------------------------------------------------
%% do_query(...)

do_query(Pid, Vsn, Debug, Query) ->
    Packet = list_to_binary([?MYSQL_QUERY_OP, Query]),
    Pid ! {send, Packet, 0},
    Response = get_query_response(Pid, Vsn),
    case Response of
	{error, Str} ->
	    io:format("Bad query:~s~nRespnse:~s~n",[Query,Str]);
	_ ->
	    debug(Debug, "Query=~p~nResponse=~p~n",[Query, Response])
    end,
    Response.

debug(false, _, _)        -> void;
debug(true, Format, Data) -> io:format(Format, Data).

get_query_response(Pid, Vsn) ->
    <<Fieldcount:8, Rest/binary>>  = do_recv(Pid),
    %% io:format("Fieldcount:~p~n",[Fieldcount]),
    case Fieldcount of
	0 ->
	    %% No Tabular data
	    <<AffectedRows:8, _Rest2/binary>> = Rest,
	    {updated, AffectedRows};
	255 ->
	    <<_Code:16/little, Message/binary>>  = Rest,
	    {error, binary_to_list(Message)};
	_ ->
	    %% Tabular data received
	    Fields = get_fields(Pid, [], Vsn),
	    %% io:format("Fields=~p~n",[Fields]),
	    Rows = get_rows(Fieldcount, Pid, []),
	    {data, Fields, Rows}
    end.

get_fields(Pid, Res, my_sql_40) ->
    Packet = do_recv(Pid),
    case Packet of
	<<254:8>> ->
	    reverse(Res);
	<<254:8, Rest/binary>> when size(Rest) < 8 ->
	    reverse(Res);
	_ ->
	    {Table, Rest} = get_with_length(Packet),
	    {Field, Rest2} = get_with_length(Rest),
	    {LengthB, Rest3} = get_with_length(Rest2),
	    LengthL = size(LengthB) * 8,
	    <<Length:LengthL/little>> = LengthB,
	    {Type, Rest4} = get_with_length(Rest3),
	    {_Flags, _Rest5} = get_with_length(Rest4),
	    Val = {binary_to_list(Table),
		   binary_to_list(Field),
		   Length,
		   %% TODO: Check on MySQL 4.0 if types are specified
		   %%       using the same 4.1 formalism and could
		   %%       be expanded to atoms:
		   binary_to_list(Type)},
	    get_fields(Pid, [Val|Res], my_sql_40)
    end;
get_fields(Pid, Res, my_sql_41) ->
    %% Support for MySQL 4.1.x and 5.x:
    Packet = do_recv(Pid),
    case Packet of
	<<254:8>> ->
	    reverse(Res);
	<<254:8, Rest/binary>> when size(Rest) < 8 ->
	    reverse(Res);
	_ ->
	    {_Catalog, Rest}   = get_with_length(Packet),
	    {Database, Rest2} = get_with_length(Rest),
	    {Table, Rest3}     = get_with_length(Rest2),
	    %% OrgTable is the real table name if Table is an alias
	    {_OrgTable, Rest4} = get_with_length(Rest3),
	    {Field, Rest5} = get_with_length(Rest4),
	    %% OrgField is the real field name if Field is an alias
	    {_OrgField, Rest6} = get_with_length(Rest5),

	    <<_Metadata:8/little, _Charset:16/little,
	     Length:32/little, Type:8/little,
	     _Flags:16/little, _Decimals:8/little,
	     _Rest7/binary>> = Rest6,

	    This = {binary_to_list(Database),
		    binary_to_list(Table),
		    binary_to_list(Field),
		    Length,
		    get_field_datatype(Type)},
	    get_fields(Pid, [This | Res], my_sql_41)
    end.

%% get_rows repeatedly receives rows until end-of-rows (254)
%%   is received. Each row has Nfields entries

get_rows(NFields, Pid, L) ->
    Packet = do_recv(Pid),
    case Packet of
	<<254:8, Rest/binary>> when size(Rest) < 8 ->
	    %% the last packet
	    reverse(L);
	_ ->
	    Row = get_row(NFields, Packet),
	    get_rows(NFields, Pid, [Row|L])
    end.

%% get_row(N, Data, L) ->
%%    unpacks exactly N values from Data

get_row(0, _) ->
    [];
get_row(N, Data) ->
    {Val, Data1} = get_with_length(Data),
    [Val|get_row(N-1, Data1)].

get_with_length(<<251:8, B/binary>>)                -> {null, B};
get_with_length(<<252:8, Len:16/little, B/binary>>) -> split_binary(B, Len);
get_with_length(<<253:8, Len:24/little, B/binary>>) -> split_binary(B, Len);
get_with_length(<<254:8, Len:64/little, B/binary>>) -> split_binary(B, Len);
get_with_length(<<Len:8, B/binary>>) when Len < 251 -> split_binary(B, Len).

%%----------------------------------------------------------------------
%% do_auth(Pid, Uaser, password) -> {ok, Version} | {error, Why}

do_auth(Pid, User, Password) ->
    receive
	{Pid, {mysql, Packet, Seq}} ->
	    {Version, Salt1, Salt2, Caps} = greeting(Packet),
	    AuthRes =
		case is_secure_connection(Caps) of
		    true ->
			do_new_auth(Pid,Seq+1,User,Password,Salt1,Salt2);
		    false ->
			do_old_auth(Pid,Seq+1,User,Password,Salt1)
		end,
	    %% io:format("AuthRes=~p~n",[AuthRes]),
	    case AuthRes of
		{ok, <<0:8, _Rest/binary>>, _RecvNum} ->
		    {ok, Version};
		{ok, <<255:8, Code:16/little, Message/binary>>, _RecvNum} ->
		    io:format("mysql_conn: init error ~p: ~p~n",
			      [Code, binary_to_list(Message)]),
		    {error, binary_to_list(Message)};
		{ok, RecvPacket, _RecvNum} ->
		    io:format("mysql_conn: init unknown error ~p~n",
			      [binary_to_list(RecvPacket)]),
		    {error, binary_to_list(RecvPacket)};
		{error, Reason} ->
		    io:format("mysql_conn: init failed receiving data : ~p~n",
			      [Reason]),
		    {error, Reason}
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.


do_old_auth(Pid, Seq, User, Password, Salt1) ->
    %% io:format("do old auth~n"),
    Auth = password_old(Password, Salt1),
    Packet2 = make_auth(User, Auth),
    send_mysql(Pid, Packet2, Seq),
    do_recv(Pid, Seq).

do_new_auth(Pid, Seq, User, Password, Salt1, Salt2) ->
    %% io:format("do new auth~n"),
    Auth = password_new(Password, Salt1 ++ Salt2),
    Packet2 = make_new_auth(User, Auth, none),
    send_mysql(Pid, Packet2, Seq),
    receive
	{Pid, {mysql, Packet3, Seq1}} ->
	    case Packet3 of
		<<254:8>> ->
		    AuthOld = password_old(Password, Salt1),
		    send_mysql(Pid, <<AuthOld/binary, 0:8>>, Seq1 + 1),
		    do_recv(Pid, Seq);
		_ ->
		    {ok, Packet3, Seq1}
	    end;
	Other ->
    	    {error, {oops, Pid, Other}}
    end.

password_new(Pwd, Salt) ->
    Hash1 = elib1_sha1:binstring(Pwd),
    Hash2 = elib1_sha1:binstring(binary_to_list(Hash1)),
    Res   = elib1_sha1:binstring(Salt ++ binary_to_list(Hash2)),
    bxor_binary(Res, Hash1).

password_old(Password, Salt) ->
    {P1, P2} = hash(Password),
    {S1, S2} = hash(Salt),
    Seed1 = P1 bxor S1,
    Seed2 = P2 bxor S2,
    List = rnd(9, Seed1, Seed2),
    {L, [Extra]} = lists:split(8, List),
    list_to_binary(lists:map(fun (E) ->
				     E bxor (Extra - 64)
			     end, L)).


%% part of do_old_auth/4, which is part of mysql_init/4
make_auth(User, Password) ->
    Caps = ?LONG_PASSWORD bor ?LONG_FLAG
	bor ?TRANSACTIONS bor ?FOUND_ROWS,
    Maxsize = 0,
    UserB = list_to_binary(User),
    PasswordB = Password,
    <<Caps:16/little, Maxsize:24/little, UserB/binary, 0:8,
    PasswordB/binary>>.

make_new_auth(User, Password, Database) ->
    DBCaps = case Database of
		 none ->
		     0;
		 _ ->
		     ?CONNECT_WITH_DB
	     end,
    Caps = ?LONG_PASSWORD bor ?LONG_FLAG bor ?TRANSACTIONS bor
	?PROTOCOL_41 bor ?SECURE_CONNECTION bor DBCaps
	bor ?FOUND_ROWS,
    Maxsize = ?MAX_PACKET_SIZE,
    UserB = list_to_binary(User),
    PasswordL = size(Password),
    DatabaseB = case Database of
		    none ->
			<<>>;
		    _ ->
			list_to_binary(Database)
		end,
    <<Caps:32/little, Maxsize:32/little, 8:8, 0:23/integer-unit:8,
    UserB/binary, 0:8, PasswordL:8, Password/binary, DatabaseB/binary>>.

hash(S) ->
    hash(S, 1345345333, 305419889, 7).

hash([C | S], N1, N2, Add) ->
    N1_1 = N1 bxor (((N1 band 63) + Add) * C + N1 * 256),
    N2_1 = N2 + ((N2 * 256) bxor N1_1),
    Add_1 = Add + C,
    hash(S, N1_1, N2_1, Add_1);
hash([], N1, N2, _Add) ->
    Mask = (1 bsl 31) - 1,
    {N1 band Mask , N2 band Mask}.

%%----------------------------------------------------------------------
%%  parse the initial greeting from mysql

greeting(Packet) ->
    <<_Protocol:8, Rest/binary>> = Packet,
    {Version, Rest2} = asciz(Rest),
    <<_TreadID:32/little, Rest3/binary>> = Rest2,
    {Salt, Rest4} = asciz(Rest3),
    <<Caps:16/little, Rest5/binary>> = Rest4,
    <<_ServerChar:16/binary-unit:8, Rest6/binary>> = Rest5,
    {Salt2, _Rest7} = asciz(Rest6),
%%%     io:format("greeting version ~p (protocol ~p) salt ~p caps ~p "
%%% 	      "serverchar ~p salt2 ~p~n",
%%% 	      [Version, Protocol, Salt, Caps, ServerChar, Salt2]),
    {normalize_version(Version), Salt, Salt2, Caps}.


normalize_version([$4,$.,$0|_T]) ->
    io:format("Switching to MySQL 4.0.x protocol.~n"),
    my_sql_40;
normalize_version([$4,$.,$1|_T]) ->
    my_sql_41;
normalize_version([$5|_T]) ->
    %% MySQL version 5.x protocol is compliant with MySQL 4.1.x:
    my_sql_41;
normalize_version(Other) ->
    io:format("MySQL version not supported: ~p "
	      "MySQL Erlang module might not work correctly.~n", [Other]),
    %% Error, but trying the oldest protocol anyway:
    my_sql_40.

%%----------------------------------------------------------------------
%% odds and ends

asciz(Data) when is_list(Data)   -> asciz_list(Data, []);
asciz(Data) when is_binary(Data) -> asciz_binary(Data, []).

asciz_list([], L)    -> {reverse(L), []};
asciz_list([0|T], L) -> {reverse(L), T};
asciz_list([H|T], L) -> asciz_list(T, [H|L]).

asciz_binary(<<>>, Acc)                 -> {reverse(Acc), <<>>};
asciz_binary(<<0:8, Rest/binary>>, Acc) -> {reverse(Acc), Rest};
asciz_binary(<<C:8, Rest/binary>>, Acc) -> asciz_binary(Rest, [C|Acc]).

rnd(N, Seed1, Seed2) ->
    Mod = (1 bsl 30) - 1,
    rnd(N, [], Seed1 rem Mod, Seed2 rem Mod).

rnd(0, List, _, _) ->
    reverse(List);
rnd(N, List, Seed1, Seed2) ->
    Mod = (1 bsl 30) - 1,
    NSeed1 = (Seed1 * 3 + Seed2) rem Mod,
    NSeed2 = (NSeed1 + Seed2 + 33) rem Mod,
    Float = (float(NSeed1) / float(Mod))*31,
    Val = trunc(Float)+64,
    rnd(N-1, [Val|List], NSeed1, NSeed2).

bxor_binary(B1, B2) ->
    list_to_binary(zipwith(fun (E1, E2) ->  E1 bxor E2 end,
			   binary_to_list(B1), binary_to_list(B2))).

send_mysql(Pid, Packet, Seq) when is_binary(Packet),
				  is_integer(Seq) ->
    Pid ! {send, Packet, Seq}.

%%--------------------------------------------------------------------
get_field_datatype(0) ->   'DECIMAL';
get_field_datatype(1) ->   'TINY';
get_field_datatype(2) ->   'SHORT';
get_field_datatype(3) ->   'LONG';
get_field_datatype(4) ->   'FLOAT';
get_field_datatype(5) ->   'DOUBLE';
get_field_datatype(6) ->   'NULL';
get_field_datatype(7) ->   'TIMESTAMP';
get_field_datatype(8) ->   'LONGLONG';
get_field_datatype(9) ->   'INT24';
get_field_datatype(10) ->  'DATE';
get_field_datatype(11) ->  'TIME';
get_field_datatype(12) ->  'DATETIME';
get_field_datatype(13) ->  'YEAR';
get_field_datatype(14) ->  'NEWDATE';
get_field_datatype(16) ->  'BIT';
get_field_datatype(246) -> 'DECIMAL';
get_field_datatype(247) -> 'ENUM';
get_field_datatype(248) -> 'SET';
get_field_datatype(249) -> 'TINYBLOB';
get_field_datatype(250) -> 'MEDIUM_BLOG';
get_field_datatype(251) -> 'LONG_BLOG';
get_field_datatype(252) -> 'BLOB';
get_field_datatype(253) -> 'VAR_STRING';
get_field_datatype(254) -> 'STRING';
get_field_datatype(255) -> 'GEOMETRY'.

is_secure_connection(Caps) ->
    case Caps band ?SECURE_CONNECTION of
	?SECURE_CONNECTION -> true;
	_ -> false
    end.

do_recv(Pid) ->
    receive
	{Pid, {mysql, Packet, _Seq}} ->
	    Packet
    end.

do_recv(Pid, Seq) ->
    receive
	{Pid, {mysql, Packet, Seq}} ->
	    {Packet, Seq};
	{Pid, Other} ->
	    io:format("unexpected data:~p ~n",[Other]),
	    exit(1)
    end.

%% Quote a string|binary|atom so that it can be included safely in a
%% mysql query

quote(X) when is_list(X) -> [$"|reverse([$"|quote(X, [])])];
quote(X) when is_binary(X) -> quote(binary_to_list(X));
quote(X) when is_atom(X) -> quote(atom_to_list(X)).

quote([], L)      -> L;
quote([0|T], L)   -> quote(T, [$0,  $\\|L]);
quote([10|T], L)  -> quote(T, [$n,  $\\|L]);
quote([13|T], L)  -> quote(T, [$r,  $\\|L]);
quote([$\\|T], L) -> quote(T, [$\\, $\\|L]);
quote([$'|T], L)  -> quote(T, [$',  $\\|L]);
quote([$"|T], L)  -> quote(T, [$",  $\\|L]);
quote([26|T], L)  -> quote(T, [$Z,  $\\|L]);
quote([H|T], L)   -> quote(T, [H|L]).
