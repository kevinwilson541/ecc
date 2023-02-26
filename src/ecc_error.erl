%%%-------------------------------------------------------------------
%%% @doc
%%% Wrapper module for the `ecc_error' application error record, for
%%% constructing instances of `ecc_error' and convenience getters to
%%% retrieve properties of this record.
%%% @end
%%%-------------------------------------------------------------------
-module(ecc_error).

-include("ecc_error.hrl").

-export([new/2, new/3, code/1, message/1, cause/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec new(Code :: ecc_error_code(), Message :: string()) -> ecc_error().
new(Code, Message) ->
    new(Code, Message, undefined).

-spec new(Code :: ecc_error_code(), Message :: string(), Cause :: term()) -> ecc_error().
new(Code, Message, Cause) ->
    #ecc_error{code = Code, message = Message, cause = Cause}.

-spec code(Err :: ecc_error()) -> ecc_error_code().
code(#ecc_error{code = Code}) ->
    Code.

-spec message(Err :: ecc_error()) -> string().
message(#ecc_error{message = Message}) ->
    Message.

-spec cause(Err :: ecc_error()) -> ecc_error_cause() | undefined.
cause(#ecc_error{cause = Cause}) ->
    Cause.
