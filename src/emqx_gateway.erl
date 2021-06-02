%%--------------------------------------------------------------------
%% Copyright (c) 2017-2021 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(emqx_gateway).

-export([ start_gateways/1
        ]).

%% APIs
-export([ types/0
        , create/3
        , start/2
        , stop/2
        , delete/2
        , list/0
        ]).

types() ->
    emqx_gateway_registry:types().

list() ->
    [].

create(Id, Type, Name, Descr, RawConf) ->
    emqx_gateway_registry:create(Id, Type, Name, Descr, RawConf).

start(Id, Type) ->
    emqx_gateway_registry:start(Id, Type).

stop(Id, Type) ->
    emqx_gateway_registry:stop(Id, Type).

remove(Id, Type) ->
    emqx_gateway_registry:remove(Id, Type).
