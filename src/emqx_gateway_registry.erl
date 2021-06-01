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

-behavior(gen_server)

%% @doc
-callback init(Options :: list()) -> {error, any()} | {ok, GwState :: any()}.

%% @doc
-callback on_insta_create() -> {ok, GwInstaPid :: pid(), GwInstaState :: any()}.

%% @doc
-callback on_insta_update() -> ok.

%% @doc
-callback on_insta_destory() -> ok.

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

-type descriptor() :: #{ cbmod :: atom()  %% Calback module
                       , state :: any()
                       , atom() :: any()
                       }. %% TODO:

-spec load(AppName :: atom(), descriptor()) -> ok | {error, any()}.

load(AppName, Dscrptr) ->
    call({load, AppName, Dscrptr}).

-spec unload(AppName :: atom(), schema()) -> ok | {error, any()}.

unload(AppName) ->
    all({unload, AppName}).

%% @doc Return all registered protocol gateway implementation
-spec types() -> [atom()].
types() ->
    call(all_types).

call(Req) ->
    gen_server:call(?MODULE, Req, 5000).

%% Instances

-record(instance, {
          id   :: atom(),
          name :: string(),
          type :: atom(),
          order :: non_neg_integer(),
          descr :: string() | undefined,
          rawconf :: maps() = #{},
          enable = true
         }).

-type instance() :: #instance{}.

-spec list() -> [instance()].

-spec new(instance()) -> {ok, TopSupPid :: pid()}.

-spec start(Id) -> ok | {error, any()}.

-spec stop(Id) -> ok | {error, any()}.

-spec remove(Id) -> ok | {error, any()}.

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

%% TODO: Metrics ???

init([]) ->
    process_flag(trap_exit, true),

    {ok, #state{}}.

handle_call({load, AppName, Dscrptr}, _From, State = #state{types = Types}}) ->
    case maps:get(AppName, Types, notfound) of
        notfound ->
            {reply, ok, State#state{types = maps:put(AppName, Dscrptr, Types)}};
        _ ->
            {reply, {error, already_existed}, State}.
    end;

handle_call({unload, AppName}, _From,
            State = #state{types = Types,
                           running = Running,
                           stopped = Stopped}) ->
    RemoveF = fun _rmf(Insta = #instance{type = AppName}) ->
                    stop_and_delete_instance(Insta),
                    false;
                  _rmf(_) ->
                    true
              end,
    NTypes = maps:remove(AppName, Types),
    NRunning = lists:filter(RemoveF, Running),
    NStopped = lists:filter(RemoveF, Stopped),
    {reply, ok, State#state{types = NTypes,
                            running = NRunning,
                            stopped = NStopped}};

handle_call({new, Insta}, _From,
            State = #state{types = Types, running = Running}) ->
    case maps:get(Insta#instance.type, Types, notfound) of
        notfound ->
            {reply, {error, not_found}, State};
        _Dscrptr = #{cbmod := CbMod} ->
            case get_insta_status(Insta#instance.id, State) of
                notfound ->
                    try
                        %% TODO: TopState
                        {ok, InstaPid, InstaSt} =
                            CbMod:on_insta_create(
                              Insta#instance.id,
                              Insta#instance.rawconf),
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

handle_call({remove, InstaId}, _From, State) ->
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
