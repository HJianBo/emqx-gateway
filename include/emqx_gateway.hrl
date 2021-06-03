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

-ifndef(EMQX_GATEWAY_HRL).
-define(EMQX_GATEWAY_HRL, 1).

%% @doc The Gateway Instace defination
-record(instance, { id    :: atom()
                  , type  :: atom()
                  , order :: non_neg_integer()  %% ??
                  , name :: string()
                  , descr :: string() | undefined
                  , rawconf :: maps() = #{}
                  , enable = true
                  }).

-type instance() :: #instance{}.

%% @doc The instance running context
-type insta_context() ::
        #{
          %% metrics
          %% authenticators?
          %% clientinfo_override
          %% 
          %% hooks   ?
          %% pubsub  ? acl ?
         }.

%% @doc
% FIXME:
%-type clientinfo definations?
%-type conninfo definations?

-endif.
