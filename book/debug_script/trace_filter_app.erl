
f().
%% load all the modules

LogFile="trace_prelaunch.txt".
LogPath="/home/ericksun/program/rabbitmq-server-debug/rabbitmq-server-rabbitmq_prelaunch/trace_log/".
LogNum=50000.
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

Prelaunch = CurrentDir ++ "/deps/rabbit/apps/rabbitmq_prelaunch".
Dirs=[Prelaunch | Dirs1].
ListMod=[list_to_atom(filename:rootname(filename:basename(F))) || P <- Dirs, F <- filelib:wildcard(P ++ "/ebin/*.beam")].
DbgList = lists:foldl(fun(X, Sum) -> [{X, '_', fun(_) -> return_trace() end}] ++ Sum end, [], ListMod).
{ok, Dev} = file:open(FileName,[write]).
recon_trace:calls(DbgList, LogNum, [return_to, {scope, local}, {io_server, Dev}]).

