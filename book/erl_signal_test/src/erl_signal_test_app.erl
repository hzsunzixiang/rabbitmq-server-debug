-module(erl_signal_test_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	erl_signal_test_sup:start_link().

stop(_State) ->
	ok.
