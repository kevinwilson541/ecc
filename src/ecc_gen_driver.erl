%%%-------------------------------------------------------------------
%%% @doc
%%% Behavior module for defining a driver (matching a subset of gen_server
%%% semantics) for loading configuration values.
%%% @end
%%%-------------------------------------------------------------------
-module(ecc_gen_driver).

-include("ecc_gen_driver.hrl").

-export([]).

-callback start_link(Args :: term()) -> {ok, pid()} | {error, term()}.

-callback stop(Pid :: pid()) -> ok.

-callback load(Pid :: pid()) -> {ok, ecc_gen_driver_load()} | {error, term()}.
