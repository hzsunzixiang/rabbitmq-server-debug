-module(test).
-compile(export_all).
-compile(nowarn_export_all).
-include_lib("eunit/include/eunit.hrl").


boot_state_idx(stopped)      -> 0;
boot_state_idx(booting)      -> 1;
boot_state_idx(core_started) -> 2;
boot_state_idx(ready)        -> 3;
boot_state_idx(stopping)     -> 4.

is_valid(BootState) ->
    is_integer(boot_state_idx(BootState)).

start() ->
    io:format("is_valid:~p~n", [is_valid(stopped)]),
    io:format("is_valid:~p~n", [is_valid(ready)]),
    %io:format("is_valid:~p~n", [?assert(is_valid(readyx))]),
    'this is an end'.
