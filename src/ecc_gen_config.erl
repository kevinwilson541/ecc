%%%-------------------------------------------------------------------
%%% @doc
%%% Behavior module for defining a namespaced configuration compiler.
%%% @end
%%%-------------------------------------------------------------------
-module(ecc_gen_config).

-include("ecc_gen_config.hrl").

-export([]).

-callback namespace() -> ecc_gen_config_namespace().

-callback load(Config :: #{string() => string()}) ->
    {ok, ecc_gen_config_struct()} | {error, term()}.

-callback get(Key :: ecc_gen_config_key(), Config :: ecc_gen_config_struct()) ->
    ecc_gen_config_attribute() | undefined.
