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

%% @doc The gateway instance management
%%       - How to create/update/remove/start/stop it
%%       - Management authenticators
%%       - ClientInfo Override? Translators ? Mountpoint ??
%%
%% Interface:
%%      
%%      -type clientinfo :: #{'$gateway': #{authenticators: allow_anonymouse | ChainId}
%%                           }
%%       - emqx_gateway:authenticate(Type, InstaId, ClientInfo)
%%       - emqx_gateway:register(Type, InstaId, ClientInfo)
%%

-module(emqx_gateway_insta_sup).

-behaviour(gen_server).

%% APIs
-export([start_link/0]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-record(state, {}).

%%--------------------------------------------------------------------
%% APIs
%%--------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init([]) ->

    %% Input:
    %%  - CM Processer
    %%  - Metrics Counter
    %%  - GatewayInstace
    %%

    %% SideEffict:
    %%
    %%  - Call back on_insta_create/3
    %%  - 
    %%

    %% 1. Create Auth Chain
    %% 2. ClientInfo Override Fun??
    %% 3. Callback to create instance
    %% 2. Metrics
    %% 3. Registry
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal funcs
%%--------------------------------------------------------------------
