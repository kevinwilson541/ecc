%%%-------------------------------------------------------------------
%%% @doc
%%% Wrapper module for the `ecc_env_error' driver error record, for
%%% constructing instances of `ecc_env_error' and convenience getters to
%%% retrieve properties of this record. Used to define error types from
%%% the `ecc_env_driver' driver module.
%%% @end
%%%-------------------------------------------------------------------
-module(ecc_env_error).

-include("ecc_env_error.hrl").

-export([new/2, new/3, code/1, message/1, cause/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec new(Code :: ecc_env_error_code(), Message :: string()) -> ecc_env_error().
new(Code, Message) ->
    new(Code, Message, undefined).

-spec new(Code :: ecc_env_error_code(), Message :: string(), Cause :: term()) -> ecc_env_error().
new(Code, Message, Cause) ->
    #ecc_env_error{code = Code, message = Message, cause = Cause}.

-spec code(Err :: ecc_env_error()) -> ecc_env_error_code().
code(#ecc_env_error{code = Code}) ->
    Code.

-spec message(Err :: ecc_env_error()) -> string().
message(#ecc_env_error{message = Message}) ->
    Message.

-spec cause(Err :: ecc_env_error()) -> ecc_env_error_cause() | undefined.
cause(#ecc_env_error{cause = Cause}) ->
    Cause.
