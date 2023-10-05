-module(erlang_systemd_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 5,
                 period => 3600},
    ChildSpecs = [child_static(frequency)],
    {ok, {SupFlags, ChildSpecs}}.

child_static(Module) ->
    {Module, {Module, start_link, []},
     permanent, 5000, worker, [Module]}.
