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

-module(emqx_stomp_impl).

-include_lib("emqx_gateway/include/emqx_gateway.hrl").

-behavior(emqx_gateway_impl).

%% APIs
-export([ load/0
        , unload/0
        ]).

%% APIs
load() ->
    %% FIXME: Is the following concept belong to gateway??? 
    %%       >> No
    %%
    %% emqx_stomp_schema module is
    %% the schema file from emqx_stomp/priv/schema/emqx_stomp_schema.erl
    %%
    %% It's aim is to parse configurations:
    %% ```
    %% stomp.$name {
    %%   max_frame_size: 1024
    %%   ...
    %%
    %%   authenticators: [...]
    %%
    %%   listeners: [...]
    %% }
    %% ```
    %%
    %%  Conf + Schema => RuntimeOptions
    %%  Conf + Schema => APIs's Descriptor
    %%  Schema        => APIs's Descriptor
    %%  Schema        => APIs's Validator
    %%
    SchemaF = fun emqx_stomp_schema:consult/1,
    RegistryOptions = [{schema, emqx_stomp_schema}],

    YourOptions = [param1, param2],
    emqx_gateway_registry:load(?MODULE, RegistryOptions, YourOptions).

unload() ->
    emqx_gateway_registry:unload(?MODULE).

init([param1, param2]) ->
    GwState = #{},
    {ok, GwState}.

%%--------------------------------------------------------------------
%% emqx_gateway_registry callbacks
%%--------------------------------------------------------------------

on_insta_create(Insta, GwState) ->
    {ok, GwInstPid} = emqx_stomp_sup:create(Name, Options),
    {ok, GwInstPid, GwInstaState}.

on_insta_update(NewInsta, OldInstace, GwInstaState, GwState) ->
    %% XXX:
    ok.

on_insta_destroy(Insta, GwInstaState, GwState) ->
    %% XXX:
    emqx_stomp_sup:delete(Id).
