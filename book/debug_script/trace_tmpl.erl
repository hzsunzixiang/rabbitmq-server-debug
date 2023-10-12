
f().
%% load all the modules

LogFile="trace_prelaunch1012.1.txt".
LogPath="/home/ericksun/program/rabbitmq-server-debug/rabbitmq-server-rabbitmq_prelaunch/trace_log/".
LogNum=50000
LP = fun() -> [code:ensure_loaded(list_to_atom(filename:rootname(filename:basename(F)))) || P <- code:get_path(), F <- filelib:wildcard(P ++ "/*.beam")] end.
LP().  %% sync

FileName= string:concat(LogPath, LogFile).
file:delete(FileName).
file:make_dir(LogPath).


DirTmp=[aten, base64url, credentials_obfuscation, cuttlefish, enough, gen_batch_server, getopt, observer_cli, osiris, rabbit, rabbit_common, rabbitmq_codegen, ranch, recon, seshat, syslog, sysmon_handler, systemd, thoas].

DirDel=[aten, base64url, credentials_obfuscation, cuttlefish, enough, gen_batch_server, getopt, observer_cli, osiris, rabbitmq_codegen, ranch, recon, seshat, syslog, sysmon_handler, systemd, thoas].

DirsNoPrefix=DirTmp--DirDel.


{ok, CurrentDir} = file:get_cwd().
Prefix=CurrentDir ++ "/deps/".

Dirs1 = [Prefix++atom_to_list(P) || P <- DirsNoPrefix].

%% the rabbitmq_prelaunch
Prelaunch = CurrentDir ++ "/deps/rabbit/apps/rabbitmq_prelaunch".

%(rabbit@centos7-mq1)18> Prelaunch.
%"/home/ericksun/program/rabbitmq-server-debug/rabbitmq-server-rabbitmq_prelaunch/deps/rabbit/apps/rabbitmq_prelaunch"

Dirs=[Prelaunch | Dirs1].

%(rabbit@centos7-mq1)19> Dirs.
%["/home/ericksun/program/rabbitmq-server-debug/rabbitmq-server-rabbitmq_prelaunch/deps/rabbit/apps/rabbitmq_prelaunch",
% "/home/ericksun/program/rabbitmq-server-debug/rabbitmq-server-rabbitmq_prelaunch/deps/rabbit",
% "/home/ericksun/program/rabbitmq-server-debug/rabbitmq-server-rabbitmq_prelaunch/deps/rabbit_common"]
%(rabbit@centos7-mq1)20> ListMod=[list_to_atom(filename:rootname(filename:basename(F))) || P <- Dirs, F <- filelib:wildcard(P ++ "/ebin/*.beam")].
%[rabbit_boot_state,rabbit_boot_state_sup,
% rabbit_boot_state_systemd,rabbit_boot_state_xterm_titlebar,
% rabbit_logger_fmt_helpers,rabbit_logger_json_fmt,
% rabbit_logger_std_h,rabbit_logger_text_fmt,rabbit_prelaunch,
% rabbit_prelaunch_app,rabbit_prelaunch_conf,
% rabbit_prelaunch_dist,rabbit_prelaunch_early_logging,
% rabbit_prelaunch_erlang_compat,rabbit_prelaunch_errors,
% rabbit_prelaunch_file,rabbit_prelaunch_sighandler,
% rabbit_prelaunch_sup,rabbit_app,rabbit_prelaunch_logging,
% rabbit_sup,app_utils,code_version,credit_flow,delegate,
% delegate_sup,file_handle_cache,file_handle_cache_stats,
% gen_server2|...]


ListMod=[list_to_atom(filename:rootname(filename:basename(F))) || P <- Dirs, F <- filelib:wildcard(P ++ "/ebin/*.beam")].

DbgList = lists:foldl(fun(X, Sum) -> [{X, '_', fun(_) -> return_trace() end}] ++ Sum end, [], ListMod).
{ok, Dev} = file:open(FileName,[write]).
recon_trace:calls(DbgList, LogNum, [return_to, {scope, local}, {io_server, Dev}]).


