# rabbitmq-server-debug

# 最原始的，只有若干文件，能够让 rabbitmq_prelaunch 跑起来
Sun Mar 5  03:14:01 2023 rabbitmq-server-rabbitmq_prelaunch

这个项目把日志启动起来就退出了,再往下就继续看

    %% 1. Enabled plugins file.
    %ok = rabbit_prelaunch_enabled_plugins_file:setup(Context),

    %%% 2. Feature flags registry.
    %ok = rabbit_prelaunch_feature_flags:setup(Context),

    % 3. Logging. 安装logger handler
    ok = rabbit_prelaunch_logging:setup(Context),

    %%% 4. Clustering.
    %ok = rabbit_prelaunch_cluster:setup(Context),

    %% Start Mnesia now that everything is ready.
    ?LOG_DEBUG("Starting Mnesia"),
    ok = mnesia:start(),

    %ok = rabbit_ra_systems:setup(Context),

    ?LOG_DEBUG(""),
    ?LOG_DEBUG("== Prelaunch DONE =="),

    case IsInitialPass of
        true  -> rabbit_prelaunch:initial_pass_finished();
        false -> ok
    end,

# Mon Mar 13 22:55:52 2023 rabbitmq_prelaunch-init

这个工程跟上面的差不多，该用了rabbitmq的标准目录，但是还不具备从boot启动的雏形，

这时候是直接以application参数的方式启动: start(normal, []) ->

---------
rabbitmq的启动接口是是 start() , 再在其中调用application:ensurestart()

{ok, _} = application:ensure_all_started(rabbitmq_prelaunch,
                                         StartType),
{ok, _} = application:ensure_all_started(rabbit,
                                         StartType),



# Tue Mar 14 04:39:30 2023 rabbitmq_prelaunch-cluster 

构建集群
    %%% 4. Clustering.
    ok = rabbit_prelaunch_cluster:setup(Context),

# Tue Mar 14 07:18:52 2023 rabbitmq_prelaunch-ra

构建raft 集群
    %% Start Mnesia now that everything is ready.
    ?LOG_DEBUG("Starting Mnesia"),
    ok = mnesia:start(),

    ok = rabbit_ra_systems:setup(Context),

到此为止  run_prelaunch_second_phase() 运行完毕, 开始执行 start(normal, []) 函数中的其他部分


# Tue Mar 14 07:53:57 2023 rabbitmq_prelaunch-product

第一次引入 start_it
start() ->
    %% start() vs. boot(): we want to throw an error in start().
    start_it(temporary).

还要启动 rabbit_sup
        maybe_warn_about_release_series_eol(),
        log_motd(),
        {ok, SupPid} = rabbit_sup:start_link(),


# Tue Mar 14 08:16:43 2023 rabbitmq_prelaunch-sup

跟上一个差别不大

# Wed Mar 15 04:50:29 2023 rabbitmq_prelaunch-sup-package

开始研究 rabbitmq的打包 Makefile ： packet脚本

# Sat Mar 18 01:41:50 2023 rabbitmq_prelaunch-sup-dist

开始研究 rabbitmq的版本  Makefile ： dist脚本

#

