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

-module(emqx_gateway_registry).

-include("include/emqx_gateway.hrl").

-behavior(gen_server)

%% APIs for Impl.
-export([ load/2
        , unload/1
        ]).

%% APIs for Mgmt.
-export([ types/0
        , create/3
        , start/2
        , stop/2
        , delete/2
        , list/0
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
          types   = #{},      %% #{App => Dscrptr}
          running = [],       %% [instance()]
          stopped = [],       %% [instance()]
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
                       , rgopts := registry_options(),
                       , gwopts := gateway_options(),
                       , state => any()
                       , atom() :: any()
                       }.

-spec load(RegMod:: atom(), registry_options(), list()) -> ok | {error, any()}.

load(RegMod, RgOpts, GwOpts) ->
    Dscrptr = #{ cbmod  => RegMod
               , rgopts => RgOpts
               , gwopts => GwOpts
               }
    call({load, RegMod, Dscrptr}).

-spec unload(RegMod :: atom()) -> ok | {error, any()}.

unload(RegMod) ->
    all({unload, RegMod}).

%% @doc Return all registered protocol gateway implementation
-spec types() -> [atom()].
types() ->
    call(all_types).

call(Req) ->
    gen_server:call(?MODULE, Req, 5000).

%% Instances
-spec list() -> [instance()].

-spec create(Id, Type, Name, Descr, RawConf) -> {ok, pid()} | {errpr, any()}.
create(Id, Type, Name, Descr, RawConf) ->
    create(#instance{id = Id,
                  type = Type,
                  name = Name,
                  descr = Descr,
                  rawconf = RawConf
                 }).

-spec create(instance()) -> {ok, pid()} | {error, any()}.
create(Insta) ->
    call({create, Insta}).

-spec remove(Id, Type) -> ok | {error, any()}.
remove(Id, Type) ->
    call({remove, Id, Type}).

-spec start(Id, Type) -> {ok, pid()} | {error, any()}.

-spec stop(Id, Type) -> ok | {error, any()}.


%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

%% TODO: Metrics ???

init([]) ->
    process_flag(trap_exit, true),

    {ok, #state{}}.

handle_call({load, RegMod, Dscrptr}, _From, State = #state{types = Types}}) ->
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
            end
        _ ->
            {reply, {error, already_existed}, State}.
    end;

handle_call({unload, RegMod}, _From,
            State = #state{types = Types,
                           running = Running,
                           stopped = Stopped}) ->
    RemoveF = fun _rmf(Insta = #instance{type = RegMod}) ->
                    stop_and_delete_instance(Insta),
                    false;
                  _rmf(_) ->
                    true
              end,
    NTypes = maps:remove(RegMod, Types),
    NRunning = lists:filter(RemoveF, Running),
    NStopped = lists:filter(RemoveF, Stopped),
    {reply, ok, State#state{types = NTypes,
                            running = NRunning,
                            stopped = NStopped}};

handle_call({create, Insta}, _From,
            State = #state{types = Types, running = Running}) ->
    case maps:get(Insta#instance.type, Types, notfound) of
        notfound ->
            {reply, {error, not_found}, State};
        _Dscrptr = #{cbmod := CbMod, state := GwState} ->
            case get_insta_status(Insta#instance.id, State) of
                notfound ->
                    try
                        %% TODO: TopState
                        {ok, InstaPid, InstaSt} =
                            CbMod:on_insta_create(
                              Insta#instance.id,
                              Insta,
                              GwState
                             ),
                        NState = State#state{running = [Insta|Running]},
                        %% Link it
                        true = link(InstaPid),
                        {reply, InstaPid, State}
                    catch
                        Class : Reason : Stk ->
                            ?LOG(error, "Create a gateway instance id: ~s, "
                                        "name: ~s falied {~p, ~p}; "
                                        "stacktrace: ~p",
                                        [Insta#instance.id,
                                         Insta#instance.name, Stk])
                            {reply, {error, Reason}, State}
                    end;
                _ ->
                    {reply, {error, already_existed}, State}
            end
    end;
    {reply, ok, State};

handle_call({start, InstaId}, _From,
            State = #state{types = Types,
                           running = Running,
                           stopped = Stopped}) ->
    case get_insta_status(InstaId, State) of
        notfound ->
            {reply, {error, not_found}, State};
        {running, _} ->
            {reply, {error, already_started}, State};
        {stopped, Insta} ->
            Dscptor = maps:get(Insta#instance.type, Types),
            %% TODO: Create
            {ok, InstaPid, InstaSt} = cb_create(Insta, Dscptor),
            NState = State#state{stopped = Stopped -- [Insta],
                                 running = [Insta#instance{enable = true} | Running]
                                }
            {reply, {ok, InstaPid}, NState}
    end;

handle_call({stop, InstaId}, _From, State) ->
    case get_insta_status(InstaId, State) of
        {running, Insta} ->
            _ = cb_destory(Insta, State),
            %% Move to stopped
            {reply, ok, State#state{}};
        _ ->
            {reply, ok, State}
    end;

handle_call({remove, InstaId, Type}, _From, State) ->
    %% Delete it
    {reply, ok, State};

handle_call(which_children, _From, State) ->
    %% TODO:
    {reply, ok, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', From, Reason}, State) ->
    %% TODO:
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal funcs
%%--------------------------------------------------------------------

stop_and_delete_instance(Insta) ->
    %% TODO:
    ok.

get_insta_status(Id, #state{running = Running, stopped = Stopped}) ->
    case {lists:keyfind(Id, #instance.id, Running),
          lists:keyfind(Id, #instance.id, Stopped)} of
        {false, false} -> notfound;
        {false, Insta} -> {stopped, Insta};
        {Insta, false} -> {running, Insta};
    end.
