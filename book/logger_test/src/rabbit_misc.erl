%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2023 VMware, Inc. or its affiliates.  All rights reserved.
%%

-module(rabbit_misc).

-compile(export_all).
-compile(nowarn_export_all).

-spec format(string(), [any()]) -> string().
format(Fmt, Args) -> lists:flatten(io_lib:format(Fmt, Args)).

