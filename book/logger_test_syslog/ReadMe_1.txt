

5> logger:get_config().
#{primary =>
      #{level => info,
        filters =>
            [{discarded_messages,{#Fun<early_logging.0.94266441>,stop}},
             {progress_reports,{fun logger_filters:progress/2,stop}}],
        filter_default => log,metadata => #{}},
  proxy =>
      #{burst_limit_enable => false,drop_mode_qlen => 1000,
        flush_qlen => 5000,sync_mode_qlen => 500,
        overload_kill_restart_after => 5000,
        burst_limit_max_count => 500,
        burst_limit_window_time => 1000,
        overload_kill_enable => false,
        overload_kill_mem_size => 3000000,
        overload_kill_qlen => 20000},
  handlers =>
      [#{id => rmq_1_file_1,module => rabbit_logger_std_h,
         config =>
             #{type => file,
               file => "/var/log/rabbitmq/rabbit@centos7-mq1.log",
               burst_limit_enable => true,drop_mode_qlen => 200,
               flush_qlen => 1000,sync_mode_qlen => 10,
               overload_kill_restart_after => 5000,
               burst_limit_max_count => 500,
               burst_limit_window_time => 1000,
               overload_kill_enable => false,
               overload_kill_mem_size => 3000000,
               overload_kill_qlen => 20000,compress_on_rotate => false,
               file_check => 0,max_no_bytes => infinity,max_no_files => 0,
               modes => [delayed_write,raw,append],
               filesync_repeat_interval => 5000,rotate_on_date => false},
         level => info,
         filters =>
             [{progress_reports,{fun logger_filters:progress/2,stop}},
              {rmqlog_filter,{#Fun<prelaunch_logging.10.59394467>,
                              #{global => info}}}],
         filter_default => log,
         formatter =>
             {rabbit_logger_text_fmt,#{single_line => false,use_colors => false}}}],
  module_levels => []}


7> logger:get_primary_config().
#{level => info,
  filters =>
      [{discarded_messages,{#Fun<early_logging.0.94266441>,stop}},
       {progress_reports,{fun logger_filters:progress/2,stop}}],
  filter_default => log,metadata => #{}}


    ok = logger:add_handler_filter(
           default, ?FILTER_NAME, {fun filter_log_event/2, FilterConfig1}),

9> logger:get_handler_config().
[#{id => rmq_1_file_1,module => rabbit_logger_std_h,
   config =>
       #{type => file,
         file => "/var/log/rabbitmq/rabbit@centos7-mq1.log",
         burst_limit_enable => true,drop_mode_qlen => 200,
         flush_qlen => 1000,sync_mode_qlen => 10,
         overload_kill_restart_after => 5000,
         burst_limit_max_count => 500,
         burst_limit_window_time => 1000,
         overload_kill_enable => false,
         overload_kill_mem_size => 3000000,
         overload_kill_qlen => 20000,compress_on_rotate => false,
         file_check => 0,max_no_bytes => infinity,max_no_files => 0,
         modes => [delayed_write,raw,append],
         filesync_repeat_interval => 5000,rotate_on_date => false},
   level => info,
   filters =>
       [{progress_reports,{fun logger_filters:progress/2,stop}},
        {rmqlog_filter,{#Fun<prelaunch_logging.10.59394467>,
                        #{global => info}}}],
   filter_default => log,
   formatter =>
       {rabbit_logger_text_fmt,#{single_line => false,use_colors => false}}}]

11> logger:get_proxy_config().
#{burst_limit_enable => false,drop_mode_qlen => 1000,
  flush_qlen => 5000,sync_mode_qlen => 500,
  overload_kill_restart_after => 5000,
  burst_limit_max_count => 500,
  burst_limit_window_time => 1000,
  overload_kill_enable => false,
  overload_kill_mem_size => 3000000,
  overload_kill_qlen => 20000}

15> logger:get_handler_ids().
[rmq_1_file_1]

