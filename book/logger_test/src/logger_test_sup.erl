-module(logger_test_sup).
-behaviour(supervisor).
-include_lib("kernel/include/logger.hrl").
-include_lib("logging.hrl").

-export([start_link/0]).
-export([init/1]).
-compile(export_all).
-compile(nowarn_export_all).

-define(CONFIGURED_KEY, {?MODULE, configured}).

-define(Context, #{os_type=>{unix,linux}, var_origins=>#{os_type=>default, sys_prefix=>default, log_levels=>default, rabbitmq_home=>default, interactive_shell=>default, output_supports_colors=>default, conf_env_file=>default}, data_dir=>"/var/lib/rabbitmq", config_base_dir=>"/etc/rabbitmq", sys_prefix=>[], log_levels=>undefined, rabbitmq_home=>"/home/ericksun/program/rabbitmq-server-debug/rabbitmq-server-rabbitmq_prelaunch", interactive_shell=>false, output_supports_colors=>true, conf_env_file=>"/etc/rabbitmq/rabbitmq-env.conf"}).

start_link() ->
    prepare_logger(),
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [],
	{ok, {{one_for_one, 1, 5}, Procs}}.

prepare_logger() ->
    setup_early_logging(#{log_levels := undefined} = ?Context),
    finish.

setup_early_logging(#{log_levels := undefined} = Context) ->
    setup_early_logging(Context#{log_levels => get_default_log_level()});
setup_early_logging(Context) ->
    case is_configured() of
        true  -> ok;
        false -> do_setup_early_logging(Context)
    end.

is_configured() ->
    persistent_term:get(?CONFIGURED_KEY, false).

get_default_log_level() ->
    #{"prelaunch" => notice}.

do_setup_early_logging(#{log_levels := LogLevels} = Context) ->
    add_rmqlog_filter(LogLevels),
    ok = logger:update_handler_config(
           default, main_handler_config(Context)).

%main_handler_config(Context) ->
%    #{formatter=>{rabbit_logger_text_fmt,#{single_line => false,use_colors => false}}, filter_default=>log}.
%
main_handler_config(Context) ->
    #{filter_default => log,
      formatter => default_formatter(Context)}.

default_formatter(#{log_levels := #{json := true}} = Context) ->
    SingleLine = format_msgs_as_single_lines(Context),
    {rabbit_logger_json_fmt, #{single_line => SingleLine}};
default_formatter(Context) ->
    Color = use_colored_logging(Context),
    SingleLine = format_msgs_as_single_lines(Context),
    {rabbit_logger_text_fmt, #{use_colors => Color,
                               single_line => SingleLine}}.

%% rabbit_prelaunch_early_logging:use_colored_logging(#{os_type=>{unix,linux}, var_origins=>#{os_type=>default, log_levels=>default, interactive_shell=>default, output_supports_colors=>default}, log_levels=>#{"prelaunch"=>notice}, interactive_shell=>false, output_supports_colors=>true})
%
%use_colored_logging() ->
%    use_colored_logging(rabbit_prelaunch:get_context()).

use_colored_logging(#{log_levels := #{color := true},
                      output_supports_colors := true}) ->
    true;
use_colored_logging(_) ->
    false.

format_msgs_as_single_lines(#{log_levels := #{single_line := true}}) ->
    true;
format_msgs_as_single_lines(_) ->
    false.

add_primary_filters() ->
    ok = logger:add_primary_filter(
          progress_reports, {fun logger_filters:progress/2, stop}),
    ok = logger:add_primary_filter(
          discarded_messages, {fun filter_discarded_message/2, stop}).

add_rmqlog_filter(LogLevels) ->
    ok = add_primary_filters(),
    FilterConfig0 = lists:foldl(
                      fun
                          ({_, V}, FC) when is_boolean(V) -> FC;
                          ({K, V}, FC) when is_atom(K) -> FC#{K => V};
                          ({K, V}, FC) -> FC#{list_to_atom(K) => V}
                      end, #{}, maps:to_list(LogLevels)),
    FilterConfig1 = case maps:is_key(global, FilterConfig0) of
                        true  -> FilterConfig0;
                        false -> FilterConfig0#{global => ?DEFAULT_LOG_LEVEL}
                    end,
    ok = logger:add_handler_filter(
           default, ?FILTER_NAME, {fun filter_log_event/2, FilterConfig1}),
    ok = logger:set_primary_config(level, all),
    ok = persistent_term:put(?CONFIGURED_KEY, true).


%23:36:38.469958 <0.167.0> rabbit_prelaunch_early_logging:filter_log_event(#{meta=>#{line=>80, pid=><0.167.0>, time=>1697168198346474, file=>"rabbit_prelaunch.erl", gl=><0.165.0>, domain=>[rabbitmq,prelaunch], mfa=>{rabbit_prelaunch,do_run,0}}, msg=>{string,[]}, level=>debug}, #{global=>info, prelaunch=>notice})


%filter_log_event( #{meta := #{domain := ?RMQLOG_DOMAIN_GLOBAL}} = LogEvent,  FilterConfig) ->
%    LogEvent.
%%    stop.
%%    ignore.
%filter_discarded_message(_LogEvent, _OnMatch) ->
%    ignore.

filter_discarded_message(#{level := error,
                           meta := #{error_logger := #{emulator := true, tag := error}},
                           msg := {"~s~n", Msg}}, OnMatch) ->
    case string:find(Msg, "Discarding message") of
        nomatch ->
            ignore;
        _ ->
            OnMatch
    end;
filter_discarded_message(_LogEvent, _OnMatch) ->
    ignore.


filter_log_event(
  #{meta := #{domain := ?RMQLOG_DOMAIN_GLOBAL}} = LogEvent,
  FilterConfig) ->
    MinLevel = get_min_level(global, FilterConfig),
    do_filter_log_event(LogEvent, MinLevel);
filter_log_event(
  #{meta := #{domain := [?RMQLOG_SUPER_DOMAIN_NAME, CatName | _]}} = LogEvent,
  FilterConfig) ->
    MinLevel = get_min_level(CatName, FilterConfig),
    do_filter_log_event(LogEvent, MinLevel);
filter_log_event(
  #{meta := #{domain := [CatName | _]}} = LogEvent,
  FilterConfig) ->
    MinLevel = get_min_level(CatName, FilterConfig),
    do_filter_log_event(LogEvent, MinLevel);
filter_log_event(LogEvent, FilterConfig) ->
    MinLevel = get_min_level(global, FilterConfig),
    do_filter_log_event(LogEvent, MinLevel).

get_min_level(global, FilterConfig) ->
    maps:get(global, FilterConfig, none);
get_min_level(CatName, FilterConfig) ->
    case maps:is_key(CatName, FilterConfig) of
        true  -> maps:get(CatName, FilterConfig);
        false -> get_min_level(global, FilterConfig)
    end.

do_filter_log_event(_, none) ->
    stop;
do_filter_log_event(#{level := Level} = LogEvent, MinLevel) ->
    case logger:compare_levels(Level, MinLevel) of
        lt -> stop;
        _  -> LogEvent
    end.


