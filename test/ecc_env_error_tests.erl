-module(ecc_env_error_tests).

-include("ecc_env_error.hrl").
-include_lib("eunit/include/eunit.hrl").

new_test() ->
    Code = ?ECC_ENV_LOAD_ERROR,
    Message = "error message",
    Err = ecc_env_error:new(Code, Message),
    ?assertMatch(#ecc_env_error{code = Code, message = Message}, Err).

new_cause_test() ->
    Code = ?ECC_ENV_LOAD_ERROR,
    Message = "error message",
    Cause = "cause message",
    Err = ecc_env_error:new(Code, Message, Cause),
    ?assertMatch(#ecc_env_error{code = Code, message = Message, cause = Cause}, Err).

code_test() ->
    Code = ?ECC_ENV_LOAD_ERROR,
    Message = "error message",
    Cause = "cause message",
    Err = ecc_env_error:new(Code, Message, Cause),
    ?assertEqual(ecc_env_error:code(Err), Code).

message_test() ->
    Code = ?ECC_ENV_LOAD_ERROR,
    Message = "error message",
    Cause = "cause message",
    Err = ecc_env_error:new(Code, Message, Cause),
    ?assertEqual(ecc_env_error:message(Err), Message).

cause_test() ->
    Code = ?ECC_ENV_LOAD_ERROR,
    Message = "error message",
    Cause = "cause message",
    Err = ecc_env_error:new(Code, Message, Cause),
    ?assertEqual(ecc_env_error:cause(Err), Cause).
