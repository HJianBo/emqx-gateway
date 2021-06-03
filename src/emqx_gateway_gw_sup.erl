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

%% @doc The Gateway Top supervisor.
%%
%%       ( 是不是也支持把这个Gateway-Top-Sup 直接丢给插件去实现，例如:
%%          RegOpts = [{supervisor, SupSpec}]. ??? 有啥好处 ???
%%
-module(emqx_gateway_gw_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([ create_insta/2
        , remove_insta/2
        , update_insta/2
        , list_insta/1
        ]).

%% Supervisor callbacks
-export([init/1]).

%%--------------------------------------------------------------------
%% APIs
%%--------------------------------------------------------------------

start_link([GatewayId]) ->
    supervisor:start_link({local, GatewayId}, ?MODULE, []).

-spec create_insta(pid(), instance()) -> {ok, GwInstaPid :: pid()} | {error, any()}.
create_insta(Sup, Insta#{id => InstaId}) ->
    case emqx_gateway_utils:find_sup_child(Sup, InstaId) of
        {ok, _Pid} -> {error, alredy_existed};
        false ->
            %% XXX: More instances options to it?
            ChildSpec = emqx_gateway_utils:childspec(
                          worker,
                          emqx_gateway_insta_sup,
                          [Insta]
                         ),
            emqx_gateway_utils:supervisor_ret(
              supervisor:start_child(Sup, ChildSpec)
             )
    end.

-spec remove_insta(pid(), InstaId :: atom()) -> ok | {error, any()}.
remove_insta(Sup, InstaId) ->
    case emqx_gateway_utils:find_sup_child(Sup, InstaId) of
        false -> ok;
        {ok, _Pid} ->
            ok = supervisor:terminate_child(Sup, InstaId),
            ok = supervisor:delete_child(Sup, InstaId)
    end.

%% Supervisor callback

%% @doc Initialize Top Supervisor for a Protocol
%%
%%
init([]) ->
    SupFlags = #{ strategy => one_for_one
                , intensity => 10
                , period => 60
                },
    ChildSpecs = [ emqx_gateway_utils:childspec(woker, emqx_gateway_cm)
                 , emqx_gateway_utils:childspec(worker, emqx_gateway_registy) %% FIXME:
                 , emqx_gateway_registy:childspec(worker, emqx_gateway_insta_sup)
                 ],
    {ok, {SupFlags, ChildSpecs}}.
