f().
LP = fun() -> [code:ensure_loaded(list_to_atom(filename:rootname(filename:basename(F)))) || P <- code:get_path(), F <- filelib:wildcard(P ++ "/*.beam")] end.
LP().  %% 同步加载，
File="trace_systemd.txt".
Path="/home/ericksun/workspace/rabbitmq-server-debug/book/systemd/erlang_systemd/trace_log/".
FileName= string:concat(Path, File).
file:delete(FileName).
file:make_dir(Path).

DirTmp=[erlang_systemd, systemd].
DirDel=[recon].

DirsNoPrefix=DirTmp--DirDel.
{ok, CurrentDir} = file:get_cwd().
Prefix=CurrentDir ++ "/deps/".
Dirs = [Prefix++atom_to_list(P) || P <- DirsNoPrefix].
ListMod1=[list_to_atom(filename:rootname(filename:basename(F))) || P <- Dirs, F <- filelib:wildcard(P ++ "/ebin/*.beam")].
ListMod=ListMod1++[socket].

DbgList = lists:foldl(fun(X, Sum) -> [{X, '_', fun(_) -> return_trace() end}] ++ Sum end, [], ListMod).
{ok, Dev} = file:open(FileName,[write]).
recon_trace:calls(DbgList, 50000, [return_to, {scope, local}, {io_server, Dev}]).

