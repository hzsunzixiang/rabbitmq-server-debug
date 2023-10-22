https://www.erlang.org/doc/apps/kernel/logger_cookbook

1> logger:i(primary).
Primary configuration:
    Level: notice
    Filter Default: log
    Filters:
        (none)
ok
2> logger:get_primary_config().
#{level => notice,filters => [],filter_default => log,
  metadata => #{}}
3> logger:i(handlers).
Handler configuration:
    Id: default
        Module: logger_std_h
        Level:  all
        Formatter:
            Module: logger_formatter
            Config:
                legacy_header: true
                single_line: false
        Filter Default: stop
        Filters:
            Id: remote_gl
                Fun: fun logger_filters:remote_gl/2
                Arg: stop
            Id: domain
                Fun: fun logger_filters:domain/2
                Arg: {log,super,[otp,sasl]}
            Id: no_domain
                Fun: fun logger_filters:domain/2
                Arg: {log,undefined,[]}
        Handler Config:
            burst_limit_enable: true
            burst_limit_max_count: 500
            burst_limit_window_time: 1000
            drop_mode_qlen: 200
            filesync_repeat_interval: no_repeat
            flush_qlen: 1000
            overload_kill_enable: false
            overload_kill_mem_size: 3000000
            overload_kill_qlen: 20000
            overload_kill_restart_after: 5000
            sync_mode_qlen: 10
            type: standard_io
ok
5>  logger:i(default).
Handler configuration:
    Id: default
        Module: logger_std_h
        Level:  all
        Formatter:
            Module: logger_formatter
            Config:
                legacy_header: true
                single_line: false
        Filter Default: stop
        Filters:
            Id: remote_gl
                Fun: fun logger_filters:remote_gl/2
                Arg: stop
            Id: domain
                Fun: fun logger_filters:domain/2
                Arg: {log,super,[otp,sasl]}
            Id: no_domain
                Fun: fun logger_filters:domain/2
                Arg: {log,undefined,[]}
        Handler Config:
            burst_limit_enable: true
            burst_limit_max_count: 500
            burst_limit_window_time: 1000
            drop_mode_qlen: 200
            filesync_repeat_interval: no_repeat
            flush_qlen: 1000
            overload_kill_enable: false
            overload_kill_mem_size: 3000000
            overload_kill_qlen: 20000
            overload_kill_restart_after: 5000
            sync_mode_qlen: 10
            type: standard_io
ok

