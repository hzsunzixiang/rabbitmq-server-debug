-module(rabbit_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-include_lib("stdlib/include/assert.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("rabbit_common/include/logging.hrl").

start_link() ->



	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [],
    ?LOG_DEBUG("Logging: testing debug log level",
               #{domain => ?RMQLOG_DOMAIN_PRELAUNCH}),
    ?LOG_INFO("Logging: testing info log level",
              #{domain => ?RMQLOG_DOMAIN_PRELAUNCH}),
    ?LOG_NOTICE("Logging: testing notice log level",
                #{domain => ?RMQLOG_DOMAIN_PRELAUNCH}),
    ?LOG_WARNING("Logging: testing warning log level",
                 #{domain => ?RMQLOG_DOMAIN_PRELAUNCH}),
    ?LOG_ERROR("Logging: testing error log level",
               #{domain => ?RMQLOG_DOMAIN_PRELAUNCH}),
    ?LOG_CRITICAL("Logging: testing critical log level",
                  #{domain => ?RMQLOG_DOMAIN_PRELAUNCH}),
    ?LOG_ALERT("Logging: testing alert log level",
               #{domain => ?RMQLOG_DOMAIN_PRELAUNCH}),
    ?LOG_EMERGENCY("Logging: testing emergency log level",
                   #{domain => ?RMQLOG_DOMAIN_PRELAUNCH}),
	{ok, {{one_for_one, 1, 5}, Procs}}.
