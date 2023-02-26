-module(ecc_driver_manager_tests).

-include("ecc_config_manager.hrl").
-include("ecc_env_driver.hrl").
-include("ecc_error.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% Test suites

start_link_badpath_test_() ->
    {ok, CWD} = file:get_cwd(),
    Path = filename:join([CWD, "test", ".env.test.noexist"]),
    process_flag(trap_exit, true),
    Result = ecc_config_manager:start_link(#ecc_config_manager_init{
        module = ecc_env_driver, args = #ecc_env_driver_init{dotenv = Path}
    }),
    ?_assertMatch({error, #ecc_error{code = ?ECC_DRIVER_LOAD_ERROR}}, Result).

register_test_() ->
    {
        setup,
        fun start/0,
        fun stop/1,
        fun register_test_run/1
    }.

register_module_error_test_() ->
    {
        setup,
        fun start_no_putenv/0,
        fun stop/1,
        fun register_module_error_test_run/1
    }.

namespace_test_() ->
    {
        setup,
        fun start/0,
        fun stop/1,
        fun namespace_test_run/1
    }.

namespace_undefined_test_() ->
    {
        setup,
        fun start/0,
        fun stop/1,
        fun namespace_undefined_test_run/1
    }.

key_test_() ->
    {
        setup,
        fun start/0,
        fun stop/1,
        fun key_test_run/1
    }.

key_undefined_test_() ->
    {
        setup,
        fun start/0,
        fun stop/1,
        fun key_undefined_test_run/1
    }.

key_namespace_undefined_test_() ->
    {
        setup,
        fun start/0,
        fun stop/1,
        fun key_namespace_undefined_test_run/1
    }.

%%% Test hooks

start() ->
    os:putenv("PROCESS_SET_KEY", "PROCESS_SET_VALUE"),
    {ok, CWD} = file:get_cwd(),
    Path = filename:join([CWD, "test", ".env.test"]),
    {ok, Pid} = ecc_config_manager:start_link(#ecc_config_manager_init{
        module = ecc_env_driver, args = #ecc_env_driver_init{dotenv = Path}
    }),
    Pid.

start_no_putenv() ->
    {ok, CWD} = file:get_cwd(),
    Path = filename:join([CWD, "test", ".env.test"]),
    {ok, Pid} = ecc_config_manager:start_link(#ecc_config_manager_init{
        module = ecc_env_driver, args = #ecc_env_driver_init{dotenv = Path}
    }),
    Pid.

stop(_Pid) ->
    os:unsetenv("PROCESS_SET_KEY"),
    ecc_config_manager:stop().

%%% Test runners

register_test_run(_Pid) ->
    ok = ecc_config_manager:register(#ecc_register_module_req{module = ecc_test_config}),
    Table = ecc_config_manager:table(),
    Ns = ecc_test_config:namespace(),
    Out = ets:lookup(Table, Ns),
    ConfigMap = #{basic => "basic", process_set_key => "PROCESS_SET_VALUE"},
    ?_assertMatch(
        [#ecc_config_namespace{namespace = Ns, module = ecc_test_config, config = ConfigMap}], Out
    ).

register_module_error_test_run(_Pid) ->
    Result = ecc_config_manager:register(#ecc_register_module_req{module = ecc_test_config}),
    ?_assertMatch({error, #ecc_error{code = ?ECC_MODULE_LOAD_ERROR}}, Result).

namespace_test_run(_Pid) ->
    ok = ecc_config_manager:register(#ecc_register_module_req{module = ecc_test_config}),
    Entry = ecc_config_manager:namespace(#ecc_get_namespace_req{namespace = ecc_test_config}),
    ConfigMap = #{basic => "basic", process_set_key => "PROCESS_SET_VALUE"},
    ?_assertEqual(Entry, ConfigMap).

namespace_undefined_test_run(_Pid) ->
    Entry = ecc_config_manager:namespace(#ecc_get_namespace_req{namespace = not_loaded_module}),
    ?_assertEqual(Entry, undefined).

key_test_run(_Pid) ->
    ok = ecc_config_manager:register(#ecc_register_module_req{module = ecc_test_config}),
    Val = ecc_config_manager:key(#ecc_get_key_req{namespace = ecc_test_config, key = basic}),
    ?_assertEqual(Val, "basic").

key_undefined_test_run(_Pid) ->
    ok = ecc_config_manager:register(#ecc_register_module_req{module = ecc_test_config}),
    Val = ecc_config_manager:key(#ecc_get_key_req{namespace = ecc_test_config, key = not_defined}),
    ?_assertEqual(Val, undefined).

key_namespace_undefined_test_run(_Pid) ->
    Val = ecc_config_manager:key(#ecc_get_key_req{namespace = ecc_test_config, key = not_defined}),
    ?_assertEqual(Val, undefined).
