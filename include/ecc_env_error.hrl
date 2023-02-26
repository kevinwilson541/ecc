%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-define(ECC_ENV_READ_ERROR, ecc_env_read_error).
-define(ECC_ENV_LOAD_ERROR, ecc_env_load_error).

-type ecc_env_error_code() ::
    ?ECC_ENV_READ_ERROR
    | ?ECC_ENV_LOAD_ERROR.

-type ecc_env_error_cause() :: term().

-record(ecc_env_error, {
    code :: ecc_env_error_code(),
    message :: string(),
    cause :: ecc_env_error_cause() | undefined
}).

-type ecc_env_error() :: #ecc_env_error{}.
