%%%-------------------------------------------------------------------
%%% @doc
%%% Root-level module for the ecc application. Manages registering configuration
%%% modules that implement the `ecc_gen_config' behavior, as well as
%%% retrieving their compiled configuration values and specific attributes.
%%%
%%% @see ecc_gen_config:namespace/0
%%% @see ecc_gen_config:load/1
%%% @see ecc_gen_config:get/2
%%% @end
%%%-------------------------------------------------------------------
-module(ecc).

-include("ecc_config_manager.hrl").
-include("ecc_error.hrl").

%% API
-export([register/1, namespace/1, key/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%%-------------------------------------------------------------------
%%% @doc
%%% Registers a configuration module with the ecc application. Once registered,
%%% `namespace/1' and `key/2' can be called for this configuration module,
%%% based on the module's implementations of the `load/1' and 'get/2'
%%% callbacks.
%%%
%%% @see ecc:namespace/1
%%% @see ecc:key/2
%%% @end
%%%-------------------------------------------------------------------
-spec register(Mod :: module()) -> ok | {error, ecc_error()}.
register(Mod) ->
    ecc_config_manager:register(#ecc_register_module_req{module = Mod}).

%%%-------------------------------------------------------------------
%%% @doc
%%% Returns the configuration struct for a given registered configuration
%%% module. The input namespace, `Ns', is derived from the target config
%%% module's `namespace/0' callback implementation, from the `ecc_gen_config'
%%% behavior. The return value will be equivalent to the target config
%%% module's `load/1' callback implementation.
%%%
%%% If no config module has been registered with a namespace equivalent
%%% to `Ns', this function will return undefined.
%%%
%%% @see ecc:register/1
%%% @see ecc_gen_config:namespace/0
%%% @see ecc_gen_config:load/1
%%% @end
%%%-------------------------------------------------------------------
-spec namespace(Ns :: atom()) -> {ok, term()} | {error, ecc_error()}.
namespace(Ns) ->
    ecc_config_manager:namespace(#ecc_get_namespace_req{namespace = Ns}).

%%%-------------------------------------------------------------------
%%% @doc
%%% Returns a target attribute of the configuration struct for a given
%%% registered configuration module. The input namespace, `Ns', is derived
%%% from this config module's `namespace/0' callback implementation, from
%%% the `ecc_gen_config' behavior. The return value will be equivalent to
%%% the target config module's `get/2' callback implementation, passing in
%%% `Ns' and `Key'.
%%%
%%% If no config module has been registered with a namespace equivalent to `Ns',
%%% or `get/2' has no implementation for `Key', this function will return
%%% undefined.
%%%
%%% @see ecc:register/1
%%% @see ecc_gen_config:namespace/0
%%% @see ecc_gen_config:get/2
%%% @end
%%%-------------------------------------------------------------------
-spec key(Ns :: atom(), Key :: atom()) -> {ok, term()} | {error, ecc_error()}.
key(Ns, Key) ->
    ecc_config_manager:key(#ecc_get_key_req{namespace = Ns, key = Key}).
