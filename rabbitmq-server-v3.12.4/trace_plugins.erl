f().
%% load all the modules
{ok, CurrentDir} = file:get_cwd().
LogFileName = "/trace_prelaunch1026.1.txt".
LogDir = CurrentDir ++ "/trace_log".
%LogPath="/home/ericksun/program/rabbitmq-server-debug/rabbitmq_prelaunch-sup-dist/trace_log/".
LogNum=50000,
LP = fun() -> [code:ensure_loaded(list_to_atom(filename:rootname(filename:basename(F)))) || P <- code:get_path(), F <- filelib:wildcard(P ++ "/*.beam")] end.
LP().  %% sync
FileName= string:concat(LogDir, LogFileName).  %LogPath = LogDir ++ LogFileName.
file:delete(FileName).
file:make_dir(LogDir).

DirTmp=[aten, base64url, credentials_obfuscation, cuttlefish, enough, gen_batch_server, getopt, observer_cli, osiris, rabbit, rabbit_common, rabbitmq_codegen, ranch, recon, seshat, syslog, sysmon_handler, systemd, thoas, rabbitmq_prelaunch].

DirDel=[aten, base64url, credentials_obfuscation, cuttlefish, enough, gen_batch_server, getopt, observer_cli, osiris, rabbitmq_codegen, ranch, recon, seshat, syslog, sysmon_handler, systemd, thoas, rabbitmq_prelaunch].

DirsNoPrefix=DirTmp--DirDel.

{ok, CurrentDir} = file:get_cwd().
Prefix = CurrentDir ++ "/plugins/".
Dirs = [filelib:wildcard(Prefix++atom_to_list(P)++"-*") || P <- DirsNoPrefix].

%% the rabbitmq_prelaunch
%% Prelaunch = CurrentDir.
%Kernel = "/home/ericksun/install/otp-26.1/lib/erlang/lib/kernel-9.1".
%Dirs2=[Prelaunch | Dirs1].
%Dirs=[Dirs2 | [Kernel]].
%Dirs=[Prelaunch | Dirs1 ].
ListModAll=[list_to_atom(filename:rootname(filename:basename(F))) || P <- Dirs, F <- filelib:wildcard(P ++ "/ebin/*.beam")].
%ListModeAdd=[logger_backend,logger,logger_config,logger_disk_log_h,logger_filters,logger_formatter,logger_handler_watcher,logger_h_common,logger_olp,logger_proxy,logger_server,logger_simple_h,logger_std_h,logger_sup].
%ListModeAdd=[logger,logger_disk_log_h,logger_filters,logger_formatter,logger_h_common,logger_simple_h,logger_std_h].
ListModeAdd=[net_adm, rpc].
ListModeDelete=[rabbit_plugins, rabbit_misc, rabbit_file].

ListMod=(ListModAll++ListModeAdd)--ListModeDelete.
%lists:member(rabbit_env,  ListMod).
DbgList = lists:foldl(fun(X, Sum) -> [{X, '_', fun(_) -> return_trace() end}] ++ Sum end, [], ListMod).
{ok, Dev} = file:open(FileName,[write]).
recon_trace:calls(DbgList, LogNum, [return_to, {scope, local}, {io_server, Dev}]).

