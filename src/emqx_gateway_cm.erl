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

%% @doc The gateway connection management
-module(emqx_gateway_cm).

-behaviour(gen_server).

%% APIs
-export([start_link/1]).

-export([ open_session/4
        , set_chann_info/3
        , set_chan_stats/3
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-record(state, {
          gwid    :: atom(),    %% Gateway Id
          registry :: pid(),    %% ClientId Registry server
          chan_pmon :: emqx_pmon:pmon()
         }).

%%--------------------------------------------------------------------
%% APIs
%%--------------------------------------------------------------------

%% XXX: Options for cm process
start_link(Options) ->
    gen_server:start_link(?MODULE, Options, []).

-spec cmtabs(GwId) ->
cmtabs(GwId) ->
    { tabname(chan, GwId)   %% Client Tabname; Record: {ClientId, Pid}
    , tabname(conn, GwId)   %% Client ConnMod; Recrod: {{ClientId, Pid}, ConnMod}
    , tabname(info, GwId)   %% ClientInfo Tabname; Record: {{ClientId, Pid}, ClientInfo, ClientStats}
    }.

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init(Options) ->
    GwId = proplists:get_value(gwid, Options),

    TabOpts = [public, {write_concurrency, true}],

    {ChanTab, ConnTab, InfoTab} = cmtabs(GwId),
    ok = emqx_tables:new(ChanTab, [bag, {read_concurrency, true}|TabOpts]),
    ok = emqx_tables:new(ConnTab, [bag | TabOpts]),
    ok = emqx_tables:new(InfoTab, [set, compressed | TabOpts]),

    %% Start link cm-registry process
    Registry = emqx_gateway_cm_registry:start_link(GwId),

    %% Interval update stats
    %% TODO: v0.2
    %ok = emqx_stats:update_interval(chan_stats, fun ?MODULE:stats_fun/0),

    {ok, #state{chantab = ChanTab,
                conntab = ConnTab,
                infotab = InfoTab,
                registry = Registry,
                chan_pmon = emqx_pmon:new()}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({registered, {ClientId, ChanPid}}, State = #{chan_pmon := PMon}) ->
    PMon1 = emqx_pmon:monitor(ChanPid, ClientId, PMon),
    {noreply, State#state{chan_pmon = PMon1}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _MRef, process, Pid, _Reason},
            State = #{gwid = GwId, registry = Registry, chan_pmon := PMon}) ->
    ChanPids = [Pid | emqx_misc:drain_down(10000)],  %% XXX: Fixed BATCH_SIZE
    {Items, PMon1} = emqx_pmon:erase_all(ChanPids, PMon),

    CmTabs = cmtabs(GwId),
    ok = emqx_pool:async_submit(
           lists:foreach(
             fun({ChanPid, ClientId}) ->
                 do_unregister_channel({ClientId, ChanPid}, Registry, CmTabs)
             end, Items)
          ),
    {noreply, State#{chan_pmon := PMon1}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal funcs
%%--------------------------------------------------------------------

tabname(chan, GwId) ->
    list_to_atom(lists:concat([emqx_gateway_, GwId, '_channel']));
tabname(conn, GwId) ->
    list_to_atom(lists:concat([emqx_gateway_, GwId, '_channel_conn']));
tabname(info, GwId) ->
    list_to_atom(lists:concat([emqx_gateway_, GwId, '_channel_info'])).

do_unregister_channel(Chan, Registry, {ChanTab, ConnTab, InfoTab}) ->
    ok = emqx_gateway_cm_registry:unregister_channel(Registry, Chan),

    true = ets:delete(ConnTab, Chan),
    true = ets:delete(InfoTab, Chan),
    ets:delete_object(ChanTab, Chan).
