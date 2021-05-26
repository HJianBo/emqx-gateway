%%%-------------------------------------------------------------------
%% @doc emqx_gateway public API
%% @end
%%%-------------------------------------------------------------------

-module(emqx_gateway_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    emqx_gateway_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
