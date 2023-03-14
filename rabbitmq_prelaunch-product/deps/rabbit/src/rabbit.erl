-module(rabbit).
-behaviour(application).

-export([start/2]).
-export([stop/1]).
-include_lib("stdlib/include/assert.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("rabbit_common/include/logging.hrl").

start(normal, []) ->
    %% Reset boot state and clear the stop reason again (it was already
    %% made in rabbitmq_prelaunch).
    %%
    %% This is important if the previous startup attempt failed after
    %% rabbitmq_prelaunch was started and the application is still
    %% running.
    rabbit_boot_state:set(booting),
    rabbit_prelaunch:clear_stop_reason(),

    try
        run_prelaunch_second_phase(),

    %    ProductInfo = product_info(),
    %    case ProductInfo of
    %        #{product_overridden := true,
    %          product_base_name := BaseName,
    %          product_base_version := BaseVersion} ->
    %            ?LOG_INFO(
    %               "~n Starting ~s ~s on Erlang ~s [~s]~n Based on ~s ~s~n ~s~n ~s",
    %               [product_name(), product_version(), rabbit_misc:otp_release(),
    %                emu_flavor(),
    %                BaseName, BaseVersion,
    %                ?COPYRIGHT_MESSAGE, ?INFORMATION_MESSAGE],
    %               #{domain => ?RMQLOG_DOMAIN_PRELAUNCH});
    %        _ ->
    %            ?LOG_INFO(
    %               "~n Starting ~s ~s on Erlang ~s [~s]~n ~s~n ~s",
    %               [product_name(), product_version(), rabbit_misc:otp_release(),
    %                emu_flavor(),
    %                ?COPYRIGHT_MESSAGE, ?INFORMATION_MESSAGE],
    %               #{domain => ?RMQLOG_DOMAIN_PRELAUNCH})
    %    end,
    %    maybe_warn_about_release_series_eol(),
    %    log_motd(),
        {ok, SupPid} = rabbit_sup:start_link(),

    %    %% When we load plugins later in this function, we refresh feature
    %    %% flags. If `feature_flags_v2' is enabled, `rabbit_ff_controller'
    %    %% will be used. We start it now because we can't wait for boot steps
    %    %% to do this (feature flags are refreshed before boot steps run).
    %    ok = rabbit_sup:start_child(rabbit_ff_controller),

    %    %% Compatibility with older RabbitMQ versions + required by
    %    %% rabbit_node_monitor:notify_node_up/0:
    %    %%
    %    %% We register the app process under the name `rabbit`. This is
    %    %% checked by `is_running(Node)` on a remote node. The process
    %    %% is also monitord by rabbit_node_monitor.
    %    %%
    %    %% The process name must be registered *before* running the boot
    %    %% steps: that's when rabbit_node_monitor will set the process
    %    %% monitor up.
    %    %%
    %    %% Note that plugins were not taken care of at this point
    %    %% either.
    %    ?LOG_DEBUG(
    %      "Register `rabbit` process (~p) for rabbit_node_monitor",
    %      [self()]),
    %    true = register(rabbit, self()),

    %    print_banner(),
    %    log_banner(),
    %    warn_if_kernel_config_dubious(),
    %    warn_if_disc_io_options_dubious(),

    %    ?LOG_DEBUG(""),
    %    ?LOG_DEBUG("== Plugins (prelaunch phase) =="),

    %    ?LOG_DEBUG("Setting plugins up"),
    %    %% `Plugins` contains all the enabled plugins, plus their
    %    %% dependencies. The order is important: dependencies appear
    %    %% before plugin which depend on them.
    %    Plugins = rabbit_plugins:setup(),
    %    ?LOG_DEBUG(
    %      "Loading the following plugins: ~p", [Plugins]),
    %    %% We can load all plugins and refresh their feature flags at
    %    %% once, because it does not involve running code from the
    %    %% plugins.
    %    ok = app_utils:load_applications(Plugins),
    %    ok = rabbit_feature_flags:refresh_feature_flags_after_app_load(
    %           Plugins),

    %    ?LOG_DEBUG(""),
    %    ?LOG_DEBUG("== Boot steps =="),

    %    ok = rabbit_boot_steps:run_boot_steps([rabbit | Plugins]),
    %    rabbit_boot_state:set(core_started),
    %    run_postlaunch_phase(Plugins),
        {ok, SupPid}
    catch
        throw:{error, _} = Error ->
            mnesia:stop(),
            rabbit_prelaunch_errors:log_error(Error),
            rabbit_prelaunch:set_stop_reason(Error),
            rabbit_boot_state:set(stopped),
            Error;
        Class:Exception:Stacktrace ->
            mnesia:stop(),
            rabbit_prelaunch_errors:log_exception(
              Class, Exception, Stacktrace),
            Error = {error, Exception},
            rabbit_prelaunch:set_stop_reason(Error),
            rabbit_boot_state:set(stopped),
            Error
    end.


run_prelaunch_second_phase() ->
    %% Finish the prelaunch phase started by the `rabbitmq_prelaunch`
    %% application.
    %%
    %% The first phase was handled by the `rabbitmq_prelaunch`
    %% application. It was started in one of the following way:
    %%   - from an Erlang release boot script;
    %%   - from the rabbit:boot/0 or rabbit:start/0 functions.
    %%
    %% The `rabbitmq_prelaunch` application creates the context map from
    %% the environment and the configuration files early during Erlang
    %% VM startup. Once it is done, all application environments are
    %% configured (in particular `mnesia` and `ra`).
    %%
    %% This second phase depends on other modules & facilities of
    %% RabbitMQ core. That's why we need to run it now, from the
    %% `rabbit` application start function.

    %% We assert Mnesia is stopped before we run the prelaunch
    %% phases. See `rabbit_prelaunch` for an explanation.
    %%
    %% This is the second assertion, just in case Mnesia is started
    %% between the two prelaunch phases.
    %rabbit_prelaunch:assert_mnesia_is_stopped(),

    %% Get the context created by `rabbitmq_prelaunch` then proceed
    %% with all steps in this phase.
    #{initial_pass := IsInitialPass} =
    Context = rabbit_prelaunch:get_context(),

    case IsInitialPass of
        true ->
            ?LOG_DEBUG(""),
            ?LOG_DEBUG(
              "== Prelaunch phase [2/2] (initial pass) ==");
        false ->
            ?LOG_DEBUG(""),
            ?LOG_DEBUG("== Prelaunch phase [2/2] =="),
            ok
    end,

    % 1. Enabled plugins file.
    ok = rabbit_prelaunch_enabled_plugins_file:setup(Context),

    %%% 2. Feature flags registry.
    ok = rabbit_prelaunch_feature_flags:setup(Context),

    % 3. Logging.
    ok = rabbit_prelaunch_logging:setup(Context),

    %%% 4. Clustering.
    ok = rabbit_prelaunch_cluster:setup(Context),

    %% Start Mnesia now that everything is ready.
    ?LOG_DEBUG("Starting Mnesia"),
    ok = mnesia:start(),

    ok = rabbit_ra_systems:setup(Context),

    ?LOG_DEBUG(""),
    ?LOG_DEBUG("== Prelaunch DONE =="),

    case IsInitialPass of
        true  -> rabbit_prelaunch:initial_pass_finished();
        false -> ok
    end,
    ok.
stop(_State) ->
	ok.
