-module(prepare_logger_do).
-include_lib("kernel/include/logger.hrl").
-include_lib("logging.hrl").


-compile(export_all).
-compile(nowarn_export_all).

-define(Context, #{os_type=>{unix,linux}, var_origins=>#{os_type=>default, sys_prefix=>default, log_levels=>default, rabbitmq_home=>default, interactive_shell=>default, output_supports_colors=>default, conf_env_file=>default}, data_dir=>"/var/lib/rabbitmq", config_base_dir=>"/etc/rabbitmq", sys_prefix=>[], log_levels=>undefined, rabbitmq_home=>"/home/ericksun/program/rabbitmq-server-debug/rabbitmq-server-rabbitmq_prelaunch", interactive_shell=>false, output_supports_colors=>true, conf_env_file=>"/etc/rabbitmq/rabbitmq-env.conf"}).

-define(Context2, #{plugins_expand_dir=>"/var/lib/rabbitmq/mnesia/rabbit@centos7-mq1-plugins-expand", product_name=>undefined, data_dir=>"/var/lib/rabbitmq", default_vhost=>undefined, erlang_dist_tcp_port=>25672, main_log_file=>"/var/log/rabbitmq/rabbit@centos7-mq1.log", motd_file=>"/etc/rabbitmq/motd", config_base_dir=>"/etc/rabbitmq", log_base_dir=>"/var/log/rabbitmq", split_nodename=>{"rabbit","centos7-mq1"}, mnesia_base_dir=>"/var/lib/rabbitmq/mnesia", rabbitmq_home=>"/home/ericksun/program/rabbitmq-server-debug/rabbitmq-server-rabbitmq_prelaunch", additional_config_files=>"/etc/rabbitmq/conf.d/*.conf", pid_file=>"/var/lib/rabbitmq/mnesia/rabbit@centos7-mq1.pid", erlang_cookie=>undefined, nodename=>'rabbit@centos7-mq1', conf_env_file=>"/etc/rabbitmq/rabbitmq-env.conf", advanced_config_file=>"/etc/rabbitmq/advanced.config", sys_prefix=>[], dbg_output=>stdout, default_user=>undefined, enabled_plugins=>undefined, output_supports_colors=>true, os_type=>{unix,linux}, quorum_queue_dir=>"/var/lib/rabbitmq/mnesia/rabbit@centos7-mq1/quorum", log_feature_flags_registry=>false, var_origins=>#{plugins_expand_dir=>default, product_name=>default, default_vhost=>default, erlang_dist_tcp_port=>default, main_log_file=>default, motd_file=>default, log_base_dir=>default, mnesia_base_dir=>default, rabbitmq_home=>default, additional_config_files=>default, pid_file=>default, erlang_cookie=>default, nodename=>default, conf_env_file=>default, advanced_config_file=>default, sys_prefix=>default, default_user=>default, enabled_plugins=>default, output_supports_colors=>default, os_type=>default, quorum_queue_dir=>default, log_feature_flags_registry=>default, log_levels=>default, interactive_shell=>default, nodename_type=>default, keep_pid_file_on_exit=>default, product_version=>default, amqp_ipaddr=>default, plugins_path=>default, default_pass=>default, amqp_tcp_port=>default, enabled_plugins_file=>default, mnesia_dir=>default, main_config_file=>default, stream_queue_dir=>default, forced_feature_flags_on_init=>default, feature_flags_file=>default, upgrade_log_file=>default}, log_levels=>undefined, interactive_shell=>false, nodename_type=>shortnames, keep_pid_file_on_exit=>false, product_version=>undefined, amqp_ipaddr=>"auto", plugins_path=>"/home/ericksun/program/rabbitmq-server-debug/rabbitmq-server-rabbitmq_prelaunch/deps", default_pass=>undefined, amqp_tcp_port=>5672, enabled_plugins_file=>"/etc/rabbitmq/enabled_plugins", mnesia_dir=>"/var/lib/rabbitmq/mnesia/rabbit@centos7-mq1", main_config_file=>"/etc/rabbitmq/rabbitmq", stream_queue_dir=>"/var/lib/rabbitmq/mnesia/rabbit@centos7-mq1/stream", forced_feature_flags_on_init=>undefined, feature_flags_file=>"/var/lib/rabbitmq/mnesia/rabbit@centos7-mq1-feature_flags", dbg_mods=>[], upgrade_log_file=>"/var/log/rabbitmq/rabbit@centos7-mq1_upgrade.log"}).

prepare_logger() ->
    early_logging:setup_early_logging(#{log_levels := undefined} = ?Context),
    early_logging:store_context(?Context2),
    Context1 = early_logging:get_context(),
    % Logging. 安装logger handler
    ok = prelaunch_logging:setup(Context1).

