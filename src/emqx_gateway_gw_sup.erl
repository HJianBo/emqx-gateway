%%--------------------------------------------------------------------
%% Copyright (c) 2021 EMQ Technologies Co., Ltd. All Rights Reserved.
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

%% @doc The Gateway Top supervisor.
-module(emqx_gateway_gw_sup).

-behaviour(supervisor).

-include("include/emqx_gateway.hrl").

-export([start_link/1]).

-export([ create_insta/2
        , remove_insta/2
        , update_insta/2
        , start_insta/2
        , stop_insta/2
        , list_insta/1
        ]).

%% Supervisor callbacks
-export([init/1]).

%%--------------------------------------------------------------------
%% APIs
%%--------------------------------------------------------------------

start_link([GatewayId]) ->
    supervisor:start_link({local, GatewayId}, ?MODULE, [GatewayId]).

-spec create_insta(pid(), instance()) -> {ok, GwInstaPid :: pid()} | {error, any()}.
create_insta(Sup, Insta#instance{id => InstaId}) ->
    case emqx_gateway_utils:find_sup_child(Sup, InstaId) of
        {ok, _GwInstaPid} -> {error, alredy_existed};
        false ->
            %% XXX: More instances options to it?
            %%
            Ctx = ctx(Sup),
            %%
            ChildSpec = emqx_gateway_utils:childspec(
                          worker,
                          emqx_gateway_insta_sup,
                          [Insta, Ctx]
                         ),
            emqx_gateway_utils:supervisor_ret(
              supervisor:start_child(Sup, ChildSpec)
             )
    end.

-spec remove_insta(pid(), InstaId :: atom()) -> ok | {error, any()}.
remove_insta(Sup, InstaId) ->
    case emqx_gateway_utils:find_sup_child(Sup, InstaId) of
        false -> ok;
        {ok, _GwInstaPid} ->
            ok = supervisor:terminate_child(Sup, InstaId),
            ok = supervisor:delete_child(Sup, InstaId)
    end.

-spec update_insta(pid(), NewInsta :: instance()) -> ok | {error, any()}.
update_insta(Sup, NewInsta = #instance{id = InstaId}) ->
    case emqx_gateway_utils:find_sup_child(Sup, InstaId) of
        false -> {error, not_found};
        {ok, GwInstaPid} ->
            emqx_gateway_insta_sup:update(GwInstaPid, NewInsta)
    end.

-spec start_insta(pid(), atom()) -> ok | {error, any()}.
start_insta(Sup, InstaId) ->
    case emqx_gateway_utils:find_sup_child(Sup, InstaId) of
        false -> {error, not_found};
        {ok, GwInstaPid} ->
            emqx_gateway_insta_sup:start(GwInstaPid)
    end.

-spec stop_insta(pid(), atom()) -> ok | {error, any()}.
stop_insta(Sup, InstaId) ->
    case emqx_gateway_utils:find_sup_child(Sup, InstaId) of
        false -> {error, not_found};
        {ok, GwInstaPid} ->
            emqx_gateway_insta_sup:stop(GwInstaPid)
    end.

-spec list_insta(pid()) -> [instance()].
list_insta(Sup) ->
    lists:filtermap(
      fun({InstaId, GwInstaPid, _Type, _Mods}) ->
        is_gateway_insta_id(InstaId)
          andalso emqx_gateway_insta_sup:instance(GwInstaPid)
      end, supervisor:which_children(?MODULE)).

%% Supervisor callback

%% @doc Initialize Top Supervisor for a Protocol
%%
%%
init([GatewayId]) ->
    SupFlags = #{ strategy => one_for_one
                , intensity => 10
                , period => 60
                },
    CmOpts = [{gwid, GatewayId}],
    CM = emqx_gateway_utils:childspec(woker, emqx_gateway_cm, [CmOpts]),

    %emqx_gateway_utils:childspec(worker, emqx_gateway_registy) %% FIXME:
    {ok, {SupFlags, [CM]}}.

%%--------------------------------------------------------------------
%% Internal funcs
%%--------------------------------------------------------------------

ctx(Sup) ->
    CM = emqx_gateway_utils:find_sup_child(Sup, emqx_gateway_cm),
    #{cm => CM}.

is_gateway_insta_id(emqx_gateway_cm) ->
    false;
is_gateway_insta_id(Id) ->
    true.
