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

%% @doc The gateway instance context
-module(emqx_gateway_insta_ctx).

%% @doc The instance running context
-type context() ::
        #{ id   := instance_id()
         , auth := allow_anonymouse | emqx_authentication:chain_id()
         , cm   := pid()
         , metrics := metrics()
         %% metrics
         %% authenticators?
         %% clientinfo_override
         %% 
         %% hooks   ?
         %% pubsub  ? acl ?
         }.

-export([ authenticate/2
        , open_session/2
        , set_chann_info/3
        , set_chann_stats/3
        %, get_chann_info/0     %% TODO:
        %, get_chann_stat/0
        ]).

-export([ publish/2
        , subscribe/3
        ]).

-export([ recv/3
        , send/3
        ]).

%% Connect&Auth circle

-spec authenticate(Ctx, ClientInfo) -> {ok, NClientInfo} | {error, any()}.
authenticate(_Ctx = #{auth := allow_anonymouse}, ClientInfo) ->
    {ok, ClientInfo#{anonymous => true}};
authenticate(_Ctx = #{auth := ChainId}, ClientInfo0) ->
    ClientInfo = ClientInfo0#{
                   zone => undefined,
                   chain_id => ChainId
                  },
    case emqx_access_control:authenticate(ClientInfo) of
        {ok, AuthResult} ->
            {ok, mountpoint(maps:merge(ClientInfo, AuthResult))}
        {error, Reason} ->
            {error, Reason}
    end.

%% Session circle

%% @doc Register the session to the cluster.
%%      This function should be called after the client has authenticated
%%      successfully so that the client can be managed in the cluster.
%%
-spec open_session(Ctx, ClearStart, ClientInfo, ConnInfo, CreateSessionFun)
    -> {ok, #{session := Session,
              present := boolean(),
              pendings => list()
             }}
     | {error, any()}.

open_session(Ctx, false, ClientInfo, ConnInfo, CreateSessionFun) ->
    ?LOG(wanring, "clean_start=false is not supported now, "
                  "fallback to clean_start mode"),
    open_session(Ctx, true, ClientInfo, ConnInfo, CreateSessionFun);

open_session(_Ctx = #{cm := CM},
             CleanStart, ClientInfo, ConnInfo, CreateSessionFun) ->
    emqx_gateway_cm:open_session(CM, CleanStart,
                                 ClientInfo, ConnInfo, CreateSessionFun).

-spec set_chann_info(Ctx, ClientId, Info) -> boolean().
set_chann_info(_Ctx = #{cm := CM}, ClientId, Info) ->
    emqx_gateway_cm:set_chann_info(CM, ClientId, Info).

-spec set_chann_stats(Ctx, ClientId, Stats) -> boolean().
set_chann_stats(_Ctx = #{cm := CM}, ClientId, Stats) ->
    emqx_gateway_cm:set_chann_stats(CM, ClientId, Stats).

%%--------------------------------------------------------------------
%% Internal funcs
%%--------------------------------------------------------------------

mountpoint(ClientInfo = #{mountpoint := undefined}) ->
    ClientInfo;
mountpoint(ClientInfo = #{mountpoint := MountPoint}) ->
    MountPoint1 = emqx_mountpoint:replvar(MountPoint, ClientInfo),
    ClientInfo#{mountpoint := MountPoint1}.
