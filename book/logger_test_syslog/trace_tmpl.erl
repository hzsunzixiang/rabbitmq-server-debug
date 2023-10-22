f().
%% load all the modules

LogFile="trace_prelaunch1022.1.txt".
LogPath="/home/ericksun/program/rabbitmq-server-debug/book/logger_test_syslog/trace_log/".
LogNum=500000,
LP = fun() -> [code:ensure_loaded(list_to_atom(filename:rootname(filename:basename(F)))) || P <- code:get_path(), F <- filelib:wildcard(P ++ "/*.beam")] end.
LP().  %% sync

FileName= string:concat(LogPath, LogFile).
file:delete(FileName).
file:make_dir(LogPath).

DirTmp=[syslog].
DirDel=[].
DirsNoPrefix=DirTmp--DirDel.

{ok, CurrentDir} = file:get_cwd().
Prefix=CurrentDir ++ "/deps/".

Dirs1 = [Prefix++atom_to_list(P) || P <- DirsNoPrefix].

%% the rabbitmq_prelaunch
Prelaunch = CurrentDir.
%Kernel = "/home/ericksun/install/otp-26.1/lib/erlang/lib/kernel-9.1".
%Dirs2=[Prelaunch | Dirs1].
%Dirs=[Dirs2 | [Kernel]].
Dirs=[Prelaunch | Dirs1 ].

ListModAll=[list_to_atom(filename:rootname(filename:basename(F))) || P <- Dirs, F <- filelib:wildcard(P ++ "/ebin/*.beam")].

%ListModeAdd=[logger_backend,logger,logger_config,logger_disk_log_h,logger_filters,logger_formatter,logger_handler_watcher,logger_h_common,logger_olp,logger_proxy,logger_server,logger_simple_h,logger_std_h,logger_sup].
ListModeAdd=[logger,logger_disk_log_h,logger_filters,logger_formatter,logger_h_common,logger_simple_h,logger_std_h].

ListMod=ListModAll++ListModeAdd.
%lists:member(rabbit_env,  ListMod).
DbgList = lists:foldl(fun(X, Sum) -> [{X, '_', fun(_) -> return_trace() end}] ++ Sum end, [], ListMod).
{ok, Dev} = file:open(FileName,[write]).
recon_trace:calls(DbgList, LogNum, [return_to, {scope, local}, {io_server, Dev}]).


