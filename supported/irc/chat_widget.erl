%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(chat_widget).

%% naming of things
%%   chat_widget.erl    the erlang controller
%%   chat_erl.tcl       the tcl component
%%   chat_erl_test.tcl  the tcl test program
%%   chat_best.tcl      pure TCL test program 

%% The *only* commands this program should emit are those used
%% in chat_erl_test.tcl

-compile(export_all).

-on_load(loaded/0).

-import(elib1_wish, [cmd/1, cast/1, do/1]).
-import(elib1_misc, [lookup/2]).

-export([new/0,
	 loaded/0, 
	 test1/0,
	 delete_tab/2,
	 set_title/2,
	 set_button_text/3,
	 set_button_color/3,
	 add_tab/2,
	 add_content/3,
	 clear_text/2,
	 read_text/2,
	 set_handler/2]).

loaded() ->
    io:format("chat_widget loaded~n"),
    elib1_wish:start(),
    Dir = elib1_misc:root_dir() ++ "/supported/irc/",
    File = Dir ++ "chat_erl.tcl", 
    cmd("source " ++ File),
    cmd("image create photo rball -file \"" ++ Dir ++ "redball.gif\""),
    cmd("image create photo wball -file \"" ++ Dir ++ "whiteball.gif\""),
    true.

test1() ->
    Pid = new(),
    add_tab(Pid, "mnesia"),
    add_tab(Pid, "indexer"),
    add_tab(Pid, "erlang"),
    add_content(Pid, "erlang", "Erlang is great"),
    make_test_widget(Pid).

new() ->
    %% cover:start(),
    %% cover:compile(elib1_wish),
    Pid = spawn(fun() -> run() end),
    add_tab(Pid, "One"),
    Pid.
    
rpc(Pid, Q) ->
    Pid ! {self(), Q},
    receive
	{Pid, Reply} ->
	    Reply
    end.

add_tab(Pid, Name) -> rpc(Pid, {addTab, Name}).
 
set_title(Pid, Title) -> rpc(Pid, {setTitle, Title}).

set_button_text(Pid, N, Text) -> rpc(Pid, {setButtonText, N, Text}).

set_button_color(Pid, N, C)   -> rpc(Pid, {setButtonColor, N, C}).

add_content(Pid, Tab, Text)   -> rpc(Pid, {addText, Tab, Text}).

clear_text(Pid, N)            -> rpc(Pid, {clearText, N}).

delete_tab(Pid, N)            -> rpc(Pid, {deleteTab, N}).

read_text(Pid, N)             -> rpc(Pid, {readText, N}).

set_handler(Pid, Fun)         -> rpc(Pid, {setHandler, Fun}).

make_tab(Win, I) ->
    Tab = "f" ++ integer_to_list(I), 
    Name = "the" ++ Tab, 
    widget_add_tab(Win, Tab, Name),
    widget_set_color(Win,Tab,"red").
    %% widget_set_button_text(Win, I, "none"),
    %% put({text,I}, "").

i2s(I) -> integer_to_list(I).

run() ->
    process_flag(trap_exit, true),
    Win = elib1_wish:make_window(self(),"chat"),
    io:format("Window name=~p~n",[Win]),
    do(["chat::make_chat", Win]),
    %% [make_tab(Win, I) || I <- lists:seq(1,10)],
    %% widget_add_content(Win, "f1", "This is some content"),
    %% widget_set_color(Win,"f1","red"),
    %% widget_set_title(Win,"www.a.b:none"),
    %% widget_set_entry(Win,"I am the entry"),
    Tabs = [],
    Free = [1,2,3,4,5,6,7,8,9,10],
    loop(Win, 1, Tabs, Free).

%% Tabs must be numbered from 1 to 10
%% Tab one is the master tab and can never be deleted

loop(Win, Selected, Tabs, Free) ->
    io:format("loop: Win=~p Selected=~p Tabs=~p Free=~p~n",
    [Win, Selected, Tabs, Free]),
    receive
	{From, {deleteTab, Name}} ->
	    case lookup(Name, Tabs) of
		error ->
		    From ! {self(), {error, badTab}},
		    loop(Win, Selected, Tabs, Free);
		{ok, N} ->
		    widget_delete_tab(Win, N),
		    Tabs1 = lists:keydelete(Name,1,Tabs),
		    From ! {self(), ack},
		    loop(Win, Selected, Tabs1, [N|Free])
	    end;
	{From, {addTab, Name}} ->
	    case lookup(Name, Tabs) of
		{ok, _} ->
		    From ! {self(), {error, duplicateName}},
		    loop(Win, Selected, Tabs, Free);
		error ->
		    case Free of
			[] ->
			    From ! {self(), {error, tooManyTabs}},
			    loop(Win, Selected, Tabs, Free);
			[H|Free1] ->
			    widget_add_tab(Win,H,Name),
			    From ! {self(), ok},
			    Tabs1 = [{Name,H}|Tabs],
			    loop(Win, Selected, Tabs1, Free1)
		    end
		end;
	{From, {setButtonText, Name, Txt}} ->
	    case lookup(Name, Tabs) of
		error ->
		    From ! {self(), {error, badTab}};
		{ok, N} ->
		    From ! {self(), ack},
		    widget_set_button_text(Win, N, "{" ++ Txt ++ "}")
	    end,
	    loop(Win, Selected, Tabs, Free);
	{From, {setButtonColor, Name, Color}} ->
	    io:format("sbc ~p ~p~n",[Name,Color]),
	    case lookup(Name, Tabs) of
		error ->
		    From ! {self(), {error, badTab}};
		{ok, N} ->
		    From ! {self(), ack},
		    io:format("widget set color:~p~n",[{Win,N,Color}]),
		    widget_set_color(Win, N, Color)
	    end,
	    loop(Win, Selected, Tabs, Free);
	{From, {addText, Name, Txt}} ->
	    %% io:format("add text (Sel=~p) ~p ~p ~n",[Selected,Name,Txt]),
	    case lookup(Name, Tabs) of
		error ->
		    From ! {self(), {error, badTab}},
		    loop(Win, Selected, Tabs, Free);
		{ok, N} ->
		    From ! {self(), ack},
		    widget_add_content(Win, N, Txt),
		    case N of
			Selected ->
			    %% widget is on screen
			    loop(Win, Selected, Tabs, Free);
			_ ->
			    %% widget not selected
			    %% change the color of the tab to show we have
			    %% some text in the display area
			    widget_set_color(Win, N, "red"),
			    loop(Win, Selected, Tabs, Free)
		    end
	    end;
	{event, {[], "clicked f" ++ Tab}} ->
	    N = list_to_integer(Tab),
	    io:format("Clicked N=~p Selected=~p~n",[N, Selected]),
	    case N of
		Selected ->
		    %% no change
		    loop(Win, Selected, Tabs, Free);
		_ ->
		    %% we have selected another window
		    widget_set_color(Win, N, "white"),
		    loop(Win, N, Tabs, Free)
	    end;
	{event, {[], "entry " ++ E}} ->
	    io:format("Got entry:~p~n",[E]),
	    loop(Win, Selected, Tabs, Free);
	{From, Other} ->
	    From ! {self(), error},
	    loop(Win, Selected, Tabs, Free);
	Any ->
	    io:format("IRC: received:~p~n",[Any]),
	    loop(Win, Selected, Tabs,Free)
    end.

widget_add_tab(Win, N, Name) ->
    Tab = "f" ++ i2s(N),
    do(["chat::add_tab",Win,Tab,Name]).

widget_delete_tab(Win, N) ->
    do(["chat::delete_tab",Win,tname(N)]).

widget_set_button_text(Win, N, Txt) -> 
    do(["setButtonText",Win,tname(N),Txt]).

widget_set_title(Win, Title) -> do(["chat::set_title",Win,Title]).

widget_add_content(Win, Tab, Txt) -> 
    do(["chat::add_content", Win, tname(Tab), "{" ++ Txt ++ "}"]).

widget_clear_text(Win) -> do(["clearText", Win]).
    
widget_set_color(Win, Tab, Color) -> 
    do(["chat::set_color",Win,tname(Tab),Color]).

tname(N) ->
    "f" ++ integer_to_list(N).

widget_set_entry(Win, Str) ->
    do(["chat::set_entry",Win,"{" ++ Str ++ "}"]).

addListBox(Win, Txt) -> do(["addListBox", Win, "{" ++ Txt ++ "}"]).

addEditor(Win, Txt) ->
    do(["addEditor", Win, Txt]).

%%----------------------------------------------------------------------
%% 

make_test_widget(Pid) ->
    widget_lib:make_button_box(
      [{"tab erlang red",
	fun() -> set_button_color(Pid,"erlang","red") end},
       {"tab erlang white",
	fun() -> set_button_color(Pid,"erlang","white") end},
       {"add content to erlang",
	fun() -> add_content(Pid, "erlang", "\n this is some\ncontent") end},
       {"destroy tab mnesia",
	fun() -> delete_tab(Pid, "mnesia") end},
       {"make tab mnesia",
	fun() -> add_tab(Pid, "mnesia") end}
      ]).

