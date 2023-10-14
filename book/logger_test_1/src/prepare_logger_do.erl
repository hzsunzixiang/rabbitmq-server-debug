-module(prepare_logger_do).
-include_lib("kernel/include/logger.hrl").
-include_lib("logging.hrl").


-compile(export_all).
-compile(nowarn_export_all).

-define(CONFIGURED_KEY, {?MODULE, configured}).
-define(PT_KEY_CONTEXT,       {?MODULE, context}).
-define(PT_KEY_INITIAL_PASS,  {?MODULE, initial_pass_finished}).

-define(Context, #{os_type=>{unix,linux}, var_origins=>#{os_type=>default, sys_prefix=>default, log_levels=>default, rabbitmq_home=>default, interactive_shell=>default, output_supports_colors=>default, conf_env_file=>default}, data_dir=>"/var/lib/rabbitmq", config_base_dir=>"/etc/rabbitmq", sys_prefix=>[], log_levels=>undefined, rabbitmq_home=>"/home/ericksun/program/rabbitmq-server-debug/rabbitmq-server-rabbitmq_prelaunch", interactive_shell=>false, output_supports_colors=>true, conf_env_file=>"/etc/rabbitmq/rabbitmq-env.conf"}).

-define(Context2, #{plugins_expand_dir=>"/var/lib/rabbitmq/mnesia/rabbit@centos7-mq1-plugins-expand", product_name=>undefined, data_dir=>"/var/lib/rabbitmq", default_vhost=>undefined, erlang_dist_tcp_port=>25672, main_log_file=>"/var/log/rabbitmq/rabbit@centos7-mq1.log", motd_file=>"/etc/rabbitmq/motd", config_base_dir=>"/etc/rabbitmq", log_base_dir=>"/var/log/rabbitmq", split_nodename=>{"rabbit","centos7-mq1"}, mnesia_base_dir=>"/var/lib/rabbitmq/mnesia", rabbitmq_home=>"/home/ericksun/program/rabbitmq-server-debug/rabbitmq-server-rabbitmq_prelaunch", additional_config_files=>"/etc/rabbitmq/conf.d/*.conf", pid_file=>"/var/lib/rabbitmq/mnesia/rabbit@centos7-mq1.pid", erlang_cookie=>undefined, nodename=>'rabbit@centos7-mq1', conf_env_file=>"/etc/rabbitmq/rabbitmq-env.conf", advanced_config_file=>"/etc/rabbitmq/advanced.config", sys_prefix=>[], dbg_output=>stdout, default_user=>undefined, enabled_plugins=>undefined, output_supports_colors=>true, os_type=>{unix,linux}, quorum_queue_dir=>"/var/lib/rabbitmq/mnesia/rabbit@centos7-mq1/quorum", log_feature_flags_registry=>false, var_origins=>#{plugins_expand_dir=>default, product_name=>default, default_vhost=>default, erlang_dist_tcp_port=>default, main_log_file=>default, motd_file=>default, log_base_dir=>default, mnesia_base_dir=>default, rabbitmq_home=>default, additional_config_files=>default, pid_file=>default, erlang_cookie=>default, nodename=>default, conf_env_file=>default, advanced_config_file=>default, sys_prefix=>default, default_user=>default, enabled_plugins=>default, output_supports_colors=>default, os_type=>default, quorum_queue_dir=>default, log_feature_flags_registry=>default, log_levels=>default, interactive_shell=>default, nodename_type=>default, keep_pid_file_on_exit=>default, product_version=>default, amqp_ipaddr=>default, plugins_path=>default, default_pass=>default, amqp_tcp_port=>default, enabled_plugins_file=>default, mnesia_dir=>default, main_config_file=>default, stream_queue_dir=>default, forced_feature_flags_on_init=>default, feature_flags_file=>default, upgrade_log_file=>default}, log_levels=>undefined, interactive_shell=>false, nodename_type=>shortnames, keep_pid_file_on_exit=>false, product_version=>undefined, amqp_ipaddr=>"auto", plugins_path=>"/home/ericksun/program/rabbitmq-server-debug/rabbitmq-server-rabbitmq_prelaunch/deps", default_pass=>undefined, amqp_tcp_port=>5672, enabled_plugins_file=>"/etc/rabbitmq/enabled_plugins", mnesia_dir=>"/var/lib/rabbitmq/mnesia/rabbit@centos7-mq1", main_config_file=>"/etc/rabbitmq/rabbitmq", stream_queue_dir=>"/var/lib/rabbitmq/mnesia/rabbit@centos7-mq1/stream", forced_feature_flags_on_init=>undefined, feature_flags_file=>"/var/lib/rabbitmq/mnesia/rabbit@centos7-mq1-feature_flags", dbg_mods=>[], upgrade_log_file=>"/var/log/rabbitmq/rabbit@centos7-mq1_upgrade.log"}).

prepare_logger() ->
    setup_early_logging(#{log_levels := undefined} = ?Context),
    store_context(?Context2),
    Context = get_context(),
   % 3. Logging. 安装logger handler
    ok = rabbit_prelaunch_logging:setup(Context),
    finish.

store_context(Context) when is_map(Context) ->
    persistent_term:put(?PT_KEY_CONTEXT, Context).

get_context() ->
    case persistent_term:get(?PT_KEY_CONTEXT, undefined) of
        undefined -> undefined;
        Context   -> Context#{initial_pass => is_initial_pass()}
    end.

is_initial_pass() ->
    not persistent_term:get(?PT_KEY_INITIAL_PASS, false).

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


%23:36:38.469958 <0.167.0> rabbit_prelaunch_early_logging:filter_log_event(#{meta=>#{line=>80, pid=><0.167.0>, time=>1697168198346474, file=>"rabbit_prelaunch.erl", gl=><0.165.0>, domain=>[rabbitmq,prelaunch], mfa=>{rabbit_prelaunch,do_run,0}}, msg=>{string,[]}, level=>debug}, #{global=>info, prelaunch=>notice})


%filter_log_event( #{meta := #{domain := ?RMQLOG_DOMAIN_GLOBAL}} = LogEvent,  FilterConfig) ->
%    LogEvent.
%%    stop.
%%    ignore.
%filter_discarded_message(_LogEvent, _OnMatch) ->
%    ignore.

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


