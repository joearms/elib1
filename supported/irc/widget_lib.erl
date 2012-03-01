%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(widget_lib).
-compile(export_all).
-import(elib1_wish, [cmd/1, cast/1, do/1]).

test() ->
    make_button_box(
      [
       {"button box", fun() -> make_button_box_test() end},
       {"list box", fun() -> make_list_box_test() end}
      ]).

make_button_box_test() ->
    make_button_box(
      [{"one", fun() -> io:format("hi~n") end},
       {"two", fun() -> io:format("joe~n") end},
       {"How Is this", fun() -> io:format("how is this~n") end}]).

make_button_box(L) ->
    spawn(fun() -> button_controller(L) end).

button_controller(L) ->
    %% process_flag(trap_exit, true),
    elib1_wish:start(), 
    Win = elib1_wish:make_window(self(),"test"),
    io:format("Win=~p~n",[Win]),
    L1 = make_buttons(1, Win, L),
    bloop().

make_buttons(N, Win, [{Name,Fun}|T]) ->
    S = integer_to_list(N),
    B = Win ++ "." ++ S,
    elib1_wish:do(["button", B,"-text {", Name,"} -command ",
		   "\"sendToErlang {event",
		   Win,"click",S,"}\""]), 
    elib1_wish:do(["pack",B,"-fill both -expand 1"]),
    put({button,N}, Fun),
    make_buttons(N+1, Win, T);
make_buttons(_,_,[]) ->
    void.

bloop() ->
    receive
	{event, {[], "click" ++ T}} ->
	    N = list_to_integer(elib1_misc:trim(T)),
	    %% io:format("clicked on:~p~n",[N]),
	    %% io:format("get=~p~n",[get()]),
	    F = get({button,N}),
	    spawn(fun() -> F() end),
	    bloop();
	Any ->
	    io:format("Anmy=~p~n",[Any]),
	    bloop()
    end.

make_list_box_test() ->
    make_list_box(["One","two","three","four"], 
		  fun clicked_list/1).

clicked_list(X) ->
    io:format("clicked list:~p~n",[X]).

make_list_box(L, F) ->
    elib1_wish:start(), 
    Win = elib1_wish:make_window(self(),"test"),
    io:format("Win=~p~n",[Win]),
    Lb = Win ++ ".l",
    do(["listbox",Lb,"-bd 5"]),
    do(["pack ",Lb,"-fill both -expand 1"]),
    do(["bind ",Lb,"<Double-1> sendToErlang",Win,"[selection get]"]),
    lbloop().
    
lbloop() ->
    receive
	{event, "click" ++ T} ->
	    N = list_to_integer(elib1_misc:trim(T)),
	    %% io:format("clicked on:~p~n",[N]),
	    %% io:format("get=~p~n",[get()]),
	    F = get({button,N}),
	    spawn(fun() -> F() end),
	    lbloop();
	Any ->
	    io:format("Any=~p~n",[Any]),
	    lbloop()
    end.

    

