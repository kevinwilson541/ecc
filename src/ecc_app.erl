%%%-------------------------------------------------------------------
%%% @doc
%%% Module that implements the application behavior for the `ecc' application.
%%% @end
%%%-------------------------------------------------------------------

-module(ecc_app).

-behaviour(application).

-export([start/2, stop/1]).

%%%===================================================================
%%% application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
    ecc_sup:start_link().

stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
