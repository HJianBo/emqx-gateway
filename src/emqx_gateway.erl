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

%%--------------------------------------------------------------------
%% Gateway Instace APIs

%% FIXME: Map is better ???
-spec list() -> [instance()].
list() ->
    %% TODO:
    [].

%% XXX: InstaId 是不是可以自己生成(保证全集群唯一)

-spec create(atom(), atom(), binary(), binary(), map())
    -> {ok, pid()}
     | {error, any()}.
create(InstaId, Type, Name, Descr, RawConf) ->
    case emqx_gateway_registry:lookup(Type) of
        undefined -> {error, {unknown_type, Type}};
        GwState ->
            Insta = #instance{id = InstaId,
                              type = Type,
                              name = Name,
                              descr = Descr,
                              rawconf = RawConf
                             },
            emqx_gateway_sup:create_gateway_insta(Insta)
    end.

-spec remove(atom(), atom()) -> ok | {error, any()}.
remove(InstaId, Type) ->
    emqx_gateway_sup:remove(InstaId, Type).

-spec update(instance(), atom()) -> ok | {error, any()}.
update(NewInsta, Type) ->
    emqx_gateway_sup:update_gateway_insta(NewInsta, Type).

-spec start(atom(), atom()) -> ok | {error, any()}.
start(InstaId, Type) ->
    emqx_gateway_sup:start_gateway_insta(InstaId, Type).

-spec stop(atom(), atom()) -> ok | {error, any()}.
stop(InstaId, Type) ->
    emqx_gateway_sup:stop_gateway_insta(InstaId, Type).
