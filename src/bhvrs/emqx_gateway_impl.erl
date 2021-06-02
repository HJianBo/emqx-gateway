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

-module(emqx_gateway_impl).

-include("include/emqx_gateway.hrl").

%% @doc
-callback init(Options :: list()) -> {error, any()} | {ok, GwState :: any()}.

%% @doc
-callback on_insta_create(Id :: atom(), Insta :: instance(), GwState)
    -> {ok, GwInstaPid :: pid(), GwInstaState :: any()}
     %% TODO: v0.2:
     | {ok, Childspec :: supervisor:child_spec(), GwInstaState :: any()}.

%% @doc
%% TODO: How to update it?
-callback on_insta_update(Id, NewInsta, OldInsta, GwInstaState, GwState) -> ok.

%% @doc
-callback on_insta_destroy(Id, Insta, GwInstaState, GwState) -> ok.

