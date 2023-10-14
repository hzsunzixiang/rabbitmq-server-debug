-module(logger_test_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%init([]) ->
%	Procs = [],
%	{ok, {{one_for_one, 1, 5}, Procs}}.
%

init(_) ->
    SupFlags = #{strategy => rest_for_one,
                 intensity => 2,
                 period => 3600},

    ChildSpecList = [child(my_server)],

    {ok,{SupFlags, ChildSpecList}}.

child(Module) ->
    {Module, {Module, start_link, []},
     permanent, 2000, worker, [Module]}.


