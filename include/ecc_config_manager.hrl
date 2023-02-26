%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-type ecc_driver_module() :: module().

-type ecc_driver_args() :: term().

-type ecc_config_module() :: module().

-type ecc_namespace() :: atom().

-type ecc_key() :: atom().

-type ecc_config() :: term().

-type ecc_config_attribute() :: term().

-type ecc_table() :: ets:table().

-record(ecc_config_manager_init, {
    module :: ecc_driver_module(),
    args :: ecc_driver_args()
}).

-type ecc_config_manager_init() :: #ecc_config_manager_init{}.

-record(ecc_config_manager_state, {
    driver_mod :: ecc_driver_module(),
    driver :: pid(),
    config :: #{string() => string()},
    config_table :: ets:table()
}).

-type ecc_config_manager_state() :: #ecc_config_manager_state{}.

-record(ecc_config_namespace, {
    namespace :: ecc_namespace(), module :: ecc_config_module(), config :: ecc_config()
}).

-type ecc_config_namespace() :: #ecc_config_namespace{}.

-record(ecc_register_module_req, {
    module :: ecc_config_module()
}).

-type ecc_register_module_req() :: #ecc_register_module_req{}.

-record(ecc_get_namespace_req, {
    namespace :: ecc_namespace()
}).

-type ecc_get_namespace_req() :: #ecc_get_namespace_req{}.

-record(ecc_get_key_req, {
    namespace :: ecc_namespace(),
    key :: ecc_key()
}).

-type ecc_get_key_req() :: #ecc_get_key_req{}.

-record(ecc_get_table, {}).

-type ecc_get_table() :: #ecc_get_table{}.

-define(ECC_DRIVER_DEFAULT_MODULE, ecc_env_driver).
-define(ECC_DRIVER_DEFAULT_SHUTDOWN, 5000).
