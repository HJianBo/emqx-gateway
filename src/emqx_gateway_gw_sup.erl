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

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(Type, Mod), ?CHILD(Type, Mod, [])).

-define(CHILD(Type, Mod, Args),
        #{ id => Mod
         , start => {Mod, start_link, Args}
         , type => Type
         }).

start_link([GatewayId]) ->
    supervisor:start_link({local, GatewayId}, ?MODULE, []).

init([]) ->
    %% 1. Start cm/registy
    %% 2. Regsity metrics/statstic
    %% 3. 
    SupFlags = #{ strategy => one_for_one
                , intensity => 10
                , period => 60
                },
    ChildSpecs = [ ?CHILD(woker, emqx_gateway_cm)
                 , ?CHILD(worker, emqx_gateway_registy)
                 ],
    {ok, {SupFlags, ChildSpecs}}.
