%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-record(ecc_env_driver_init, {dotenv :: iolist() | undefined}).

-type ecc_env_driver_init() :: #ecc_env_driver_init{}.

-record(ecc_env_driver_state, {dotenv :: iolist() | undefined}).

-type ecc_env_driver_state() :: #ecc_env_driver_state{}.

-record(ecc_env_driver_load, {}).

-type ecc_env_driver_load() :: #ecc_env_driver_load{}.
