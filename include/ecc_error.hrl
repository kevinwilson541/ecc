%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-define(ECC_DRIVER_LOAD_ERROR, ecc_driver_load_error).
-define(ECC_DRIVER_START_ERROR, ecc_driver_start_error).
-define(ECC_MODULE_LOAD_ERROR, ecc_module_load_error).

-type ecc_error_code() ::
    ?ECC_DRIVER_LOAD_ERROR
    | ?ECC_DRIVER_START_ERROR
    | ?ECC_MODULE_LOAD_ERROR.

-type ecc_error_cause() :: term().

-record(ecc_error, {
    code :: ecc_error_code(),
    message :: string(),
    cause :: ecc_error_cause() | undefined
}).

-type ecc_error() :: #ecc_error{}.
