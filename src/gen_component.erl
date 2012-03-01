%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

%%%-------------------------------------------------------------------
%%% File    : gen_component.erl
%%% Author  : Joe Armstrong <erlang@gmail.com>
%%% Description : 
%%%
%%% Created : 25 Nov 2009 by Joe Armstrong <erlang@gmail.com>
%%%-------------------------------------------------------------------

-module(gen_component).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(env, {cdir,lib,apps}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    case {find_ecomponent_dir(), find_ecomponent_lib()} of
	{{ok, Components},{ok, Lib}} ->
	    {ok, #env{cdir=Components, lib=Lib, apps=[]}};
	{A, B} ->
	    Errors = [E ||  {error, E} <- [A,B]],
	    {stop, {eStartUpError, Errors}}
    end.

find_ecomponent_dir() ->
    case os:genenv("ECOMPONENT_HOME") of
	false ->
	    case os:getenv("HOME") of
		false ->
		    {error, eNoHomeDirectory};
		Home ->
		    {ok, Home ++ "/eComponents"}
	    end;
	Dir ->
	    {ok, Dir}
    end.

find_ecomponent_lib() ->
    case os:genenv("ECOMPONENT_LIB") of
	false ->
	    case os:getenv("HOME") of
		false ->
		    {error, eNoLibDirectory};
		Home ->
		    {ok, Home ++ "/eLibrary"}
	    end;
	Dir ->
	    {ok, Dir}
    end.

%% Interface functions

start_component(Name, Props) ->
    gen_server:call(?MODULE, {start_component, Name, Props}).

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({start_component, Name, Props}, _From, Env) ->
    Reply = ok,
    {reply, Reply, eState}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

