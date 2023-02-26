-module(ecc_env_driver_tests).

-include("ecc_env_driver.hrl").
-include("ecc_env_error.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% Test suites

load_no_dotenv_test_() ->
    {
        setup,
        fun start_no_dotenv/0,
        fun stop/1,
        fun load_no_dotenv_test_run/1
    }.

load_dotenv_test_() ->
    {
        setup,
        fun start_dotenv/0,
        fun stop/1,
        fun load_dotenv_test_run/1
    }.

load_dotenv_badpath_test_() ->
    {
        setup,
        fun start_dotenv_badpath/0,
        fun stop/1,
        fun load_dotenv_badpath_test_run/1
    }.

%%% Test hooks

start_no_dotenv() ->
    os:putenv("PROCESS_SET_ENV", "PROCESS_SET_VALUE"),
    {ok, Pid} = ecc_env_driver:start_link(),
    Pid.

start_dotenv() ->
    {ok, CWD} = file:get_cwd(),
    Path = filename:join([CWD, "test", ".env.test"]),
    {ok, Pid} = ecc_env_driver:start_link(#ecc_env_driver_init{dotenv = Path}),
    Pid.

start_dotenv_badpath() ->
    {ok, CWD} = file:get_cwd(),
    Path = filename:join([CWD, "test", ".env.test.noexist"]),
    {ok, Pid} = ecc_env_driver:start_link(#ecc_env_driver_init{dotenv = Path}),
    Pid.

stop(Pid) ->
    os:unsetenv("PROCESS_SET_ENV"),
    ecc_env_driver:stop(Pid),
    ok.

%%% Test runners

load_no_dotenv_test_run(Pid) ->
    {ok, Map} = ecc_env_driver:load(Pid),
    Value = maps:get("PROCESS_SET_ENV", Map, undefined),
    ?_assertEqual(Value, "PROCESS_SET_VALUE").

load_dotenv_test_run(Pid) ->
    {ok, Map} = ecc_env_driver:load(Pid),
    Value = maps:get("BASIC", Map, undefined),
    ?_assertEqual(Value, "basic").

load_dotenv_badpath_test_run(Pid) ->
    Result = ecc_env_driver:load(Pid),
    ?_assertMatch({error, #ecc_env_error{code = ?ECC_ENV_READ_ERROR}}, Result).
