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

-behavior(emqx_gateway_registry).

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
    emqx_gateway_registry:load(emqx_stomp, SchemaF).

unload() ->
    emqx_gateway_registry:unload(emqx_stomp).

%%--------------------------------------------------------------------
%% emqx_gateway_registry callbacks
%%--------------------------------------------------------------------

on_insta_create(Name, Options, TopState) ->
    %% XXX:
    {ok, GwInstPid} = emqx_stomp_sup:create(Name, Options),
    {ok, GwInstPid, GwState}.

on_insta_update(Name, NOptions, OldOptions, GwState, TopState) ->
    %% XXX:
    ok.

on_insta_destory(Name, Options, GwState, TopState) ->
    %% XXX:
    emqx_stomp_sup:delete(Name).
