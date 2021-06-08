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

-module(emqx_gateway_registry).

-include("include/emqx_gateway.hrl").

-behavior(gen_server).

%% APIs for Impl.
-export([ load/3
        , unload/1
        ]).

-export([ list/0
        , lookup/1
        ]).

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

-record(state, {
          types = #{} :: #{ atom() => descriptor() }
         }).

%%--------------------------------------------------------------------
%% APIs
%%--------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% Mgmt
%%--------------------------------------------------------------------

%% Types

-type registry_options() :: list().

-type gateway_options() :: list().

-type descriptor() :: #{ cbmod  := atom()
                       , rgopts := registry_options()
                       , gwopts := gateway_options()
                       , state  => any()
                       }.

-spec load(RegMod:: atom(), registry_options(), list()) -> ok | {error, any()}.

load(RegMod, RgOpts, GwOpts) ->
    Dscrptr = #{ cbmod  => RegMod
               , rgopts => RgOpts
               , gwopts => GwOpts
               },
    call({load, RegMod, Dscrptr}).

-spec unload(RegMod :: atom()) -> ok | {error, any()}.

unload(RegMod) ->
    call({unload, RegMod}).

%% @doc Return all registered protocol gateway implementation
-spec list() -> [atom()].
list() ->
    call(all).

-spec lookup(atom()) -> emqx_gateway_impl:state().
lookup(RegMod) ->
    call({lookup, RegMod}).

call(Req) ->
    gen_server:call(?MODULE, Req, 5000).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

%% TODO: Metrics ???

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{types = #{}}}.

handle_call({load, RegMod, Dscrptr}, _From, State = #state{types = Types}) ->
    case maps:get(RegMod, Types, notfound) of
        notfound ->
            try
                GwOpts = maps:get(gwopts, Dscrptr),
                {ok, GwState} = RegMod:init(GwOpts),
                NDscrptr = maps:put(state, GwState, Dscrptr),
                NTypes = maps:put(RegMod, NDscrptr, Types),
                {reply, ok, State#state{types = NTypes}}
            catch
                error : {badmatch, {error, Reason}} ->
                    {reply, {error, Reason}, State};
                Class : Reason ->
                    {reply, {error, {Class, Reason}}, State}
            end;
        _ ->
            {reply, {error, already_existed}, State}
    end;

handle_call({unload, RegMod}, _From, State = #state{types = Types}) ->
    case maps:get(RegMod, Types, undefined) of
        undefined -> ok;
        _ ->
            GwId = RegMod,
            emqx_gateway_sup:stop_all_suptree(GwId)
    end,
    {reply, ok, State};

handle_call(all, _From, State = #state{types = Types}) ->
    Reply = maps:values(Types),
    {reply, Reply, State};

handle_call({lookup, RegMod}, _From, State = #state{types = Types}) ->
    Reply = maps:get(RegMod, Types, undefined),
    {reply, Reply, State};

handle_call(Req, _From, State) ->
    logger:error("Unexpected call: ~0p", [Req]),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
