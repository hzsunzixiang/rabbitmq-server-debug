-module(distribute).
-compile(export_all).
-compile(nowarn_export_all).

-define(DistTcpPort, 25672).

start() ->
    context_to_app_env_vars_no_logging(),
    net_kernel:start(['rabbit@centos7-mq1', shortnames, 60*1000 div 4]),
    timer:sleep(10000000000),
    'this is an end'.

random(N) ->
    rand:uniform(N).


context_to_app_env_vars_no_logging() ->
    Fun = fun({App, Param, Value}) ->
                  ok = application:set_env(
                         App, Param, Value, [{persistent, true}])
          end,
    context_to_app_env_vars1(Fun).

context_to_app_env_vars1(Fun) ->
    lists:foreach(
      Fun,
      [{kernel, inet_dist_listen_min, ?DistTcpPort},
       {kernel, inet_dist_listen_max, ?DistTcpPort}]).


