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

-module(emqx_gateway_sup).

-behaviour(supervisor).

-include("include/emqx_gateway.hrl").

-export([start_link/0]).

-export([ create_gateway_insta/2
        , remove_gateway_insta/2
        , update_gateway_insta/3
        , list_gateway_insta/0
        ]).

-export([init/1]).

-define(CHILD(Type, Mod), ?CHILD(Type, Mod, [])).

-define(CHILD(Type, Mod, Args),
        #{ id => Mod
         , start => {Mod, start_link, Args}
         , type => Type
         }).

%%--------------------------------------------------------------------
%% APIs
%%--------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec create_gateway_insta(instance()) -> {ok, pid()} | {error, any()}.
create_gateway_insta(Insta) ->
    {ok, GwSup}  = ensure_gateway_suptree_ready(gatewayid(Insa)),
    emqx_gateway_gw_sup:create_insta(GwSup, Insta).

-spec remove_gateway_insta(atom(), atom()) -> ok | {error, any()}.
remove_gateway_insta(InstaId, Type) ->
    case get_gateway_sup(Type) of
        {ok, GwSup} ->
            emqx_gateway_gw_sup:remove_insta(GwSup, InstaId);
        _ -> ok
    end.

%% Supervisor callback
init([]) ->
    SupFlags = #{ strategy => one_for_one
                , intensity => 10
                , period => 60
                },
    ChildSpecs = [ ?CHILD(woker, emqx_gateway_registry)
                 ],
    {ok, {SupFlags, ChildSpecs}}.

%%--------------------------------------------------------------------
%% Internal funcs
%%--------------------------------------------------------------------

gatewayid(#instance{type = Type}) ->
    list_to_atom(lists:concat(["gateway#", Type])).

get_gateway_sup(GatewayId) ->
    case lists:keyfind(GatewayId, 1, supervisor:which_children(?MODULE)) of
        false -> false;
        {_Id, Pid, _Type, _Mods} -> {ok, Pid}
    end

ensure_gateway_suptree_ready(GatewayId) ->
    case lists:keyfind(GatewayId, 1, supervisor:which_children(?MODULE)) of
        false ->
            ChildSpec = ?CHILD(supervisor, emqx_gateway_gw_sup, [GatewayId]),
            supervisor_ret(
              supervisor:start_child(?MODULE, ChildSpec)
             );
        {_Id, Pid, _Type, _Mods} ->
            {ok, Pid}
    end.

supervisor_ret({ok, Pid, _Info}) -> {ok, Pid};
supervisor_ret(Ret) -> Ret.
