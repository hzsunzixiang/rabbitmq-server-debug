-module(distribute).
-compile(export_all).
-compile(nowarn_export_all).

-define(DistTcpPort, 25672).

start() ->
    context_to_app_env_vars_no_logging(),
    %ensure_epmd(),
    net_kernel:start(['rabbit@centos7-mq1', shortnames, 60*1000 div 4]),
    timer:sleep(10000000000),
    'this is an end'.

random(N) ->
    rand:uniform(N).


format(Fmt, Args) -> lists:flatten(io_lib:format(Fmt, Args)).

get_erl_path() ->
    ERTSDir = format("erts-~ts", [erlang:system_info(version)]),
    Bin = filename:join([code:root_dir(), ERTSDir, "bin"]),
    case os:type() of
        {win32, _} ->
            filename:join(Bin, "erl.exe");
        _ ->
            filename:join(Bin, "erl")
    end.


port_shutdown_loop(Port) ->
    receive
        {Port, {exit_status, _Rc}} -> ok;
        {Port, closed}             -> ok;
        {Port, {data, _}}          -> port_shutdown_loop(Port);
        {'EXIT', Port, Reason}     ->
            io:format("Failed to start a one-off Erlang VM to keep epmd alive: ~p", [Reason])
    after 15000 ->
        %% ensure the port is closed
        Port ! {self(), close},
        receive
            {Port, closed } -> ok
        after 5000 -> ok
        end
    end.

ensure_epmd() ->
    Exe = get_erl_path(),
    ID = random(1000000000),
    Port = open_port(
             {spawn_executable, Exe},
             [{args, ["-boot", "no_dot_erlang",
                      "-sname", format("epmd-starter-~b", [ID]),
                      "-noinput", "-s", "erlang", "halt"]},
              exit_status, stderr_to_stdout, use_stdio]),
    port_shutdown_loop(Port).

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


