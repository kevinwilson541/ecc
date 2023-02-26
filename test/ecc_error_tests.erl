-module(ecc_error_tests).

-include("ecc_error.hrl").
-include_lib("eunit/include/eunit.hrl").

new_test() ->
    Code = ?ECC_MODULE_LOAD_ERROR,
    Message = "error message",
    Err = ecc_error:new(Code, Message),
    ?assertMatch(#ecc_error{code = Code, message = Message}, Err).

new_cause_test() ->
    Code = ?ECC_MODULE_LOAD_ERROR,
    Message = "error message",
    Cause = "cause message",
    Err = ecc_error:new(Code, Message, Cause),
    ?assertMatch(#ecc_error{code = Code, message = Message, cause = Cause}, Err).

code_test() ->
    Code = ?ECC_MODULE_LOAD_ERROR,
    Message = "error message",
    Cause = "cause message",
    Err = ecc_error:new(Code, Message, Cause),
    ?assertEqual(ecc_error:code(Err), Code).

message_test() ->
    Code = ?ECC_MODULE_LOAD_ERROR,
    Message = "error message",
    Cause = "cause message",
    Err = ecc_error:new(Code, Message, Cause),
    ?assertEqual(ecc_error:message(Err), Message).

cause_test() ->
    Code = ?ECC_MODULE_LOAD_ERROR,
    Message = "error message",
    Cause = "cause message",
    Err = ecc_error:new(Code, Message, Cause),
    ?assertEqual(ecc_error:cause(Err), Cause).
