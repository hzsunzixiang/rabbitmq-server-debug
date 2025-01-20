-module(erlang_systemd_app).
%-behaviour(application).

%-export([start/2]).
-export([boot/0]).
-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    boot().

boot() ->
	erlang_systemd_sup:start_link().

stop(_State) ->
	ok.
