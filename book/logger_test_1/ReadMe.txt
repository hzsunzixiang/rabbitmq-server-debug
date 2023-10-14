

erlang logger 的使用

wget https://erlang.mk/erlang.mk
make -f erlang.mk bootstrap 

* 增加工程文件
  make new t=gen_server n=my_server 
  #make new t=gen_server n=my_server in=webchat

%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2019-2023 VMware, Inc. or its affiliates.  All rights reserved.
%%

%% @author The RabbitMQ team
%% @copyright 2019-2023 VMware, Inc. or its affiliates.
%%
%% @doc
%% This module manages the configuration of the Erlang Logger facility. In
%% other words, it translates the RabbitMQ logging configuration (in the
%% Cuttlefish format or classic Erlang-term-based configuration) into Erlang
%% Logger handler setups.
%%
%% Configuring the Erlang Logger is done in two steps:
%% <ol>
%% <li>Logger handler configurations are created based on the configuration
%% and the context (see {@link //rabbit_common/rabbit_env}).</li>
%% <li>Created handlers are installed (i.e. they become active). Any handlers
%% previously installed by this module are removed.</li>
%% </ol>
%%
%% It also takes care of setting the `$ERL_CRASH_DUMP' variable to enable
%% Erlang core dumps.
%%
%% Note that before this module handles the Erlang Logger, {@link
%% //rabbitmq_prelaunch/rabbit_prelaunch_early_logging} configures basic
%% logging to have messages logged as soon as possible during RabbitMQ
%% startup.
%%
%% == How to configure RabbitMQ logging ==
%%
%% RabbitMQ supports a main/default logging output and per-category outputs.
%% An output is a combination of a destination (a text file or stdout for
%% example) and a message formatted (e.g. plain text or JSON).
%%
%% Here is the Erlang-term-based configuration expected and supported by this
%% module:
%%
%% ```
%% {rabbit, [
%%   {log_root, string()},
%%   {log, [
%%     {categories, [
%%       {default, [
%%         {level, Level}
%%       ]},
%%       {CategoryName, [
%%         {level, Level},
%%         {file, Filename}
%%       ]}
%%     ]},
%%
%%     {console, [
%%       {level, Level},
%%       {enabled, boolean()}
%%     ]},
%%
%%     {exchange, [
%%       {level, Level},
%%       {enabled, boolean()}
%%     ]},
%%
%%     {file, [
%%       {level, Level},
%%       {file, Filename | false},
%%       {date, RotationDateSpec},
%%       {size, RotationSize},
%%       {count, RotationCount},
%%     ]},
%%
%%     {journald, [
%%       {level, Level},
%%       {enabled, boolean()},
%%       {fields, proplists:proplist()}
%%     ]}
%%
%%     {syslog, [
%%       {level, Level},
%%       {enabled, boolean()}
%%     ]}
%%   ]}
%% ]}.
%%
%% Level = logger:level().
%% Filename = file:filename().
%% RotationDateSpec = string(). % Pattern format used by newsyslog.conf(5).
%% RotationSize = non_neg_integer() | infinity.
%% RotationCount = non_neg_integer().
%% '''
%%
%% See `priv/schema/rabbit.schema' for the definition of the Cuttlefish
%% configuration schema.
