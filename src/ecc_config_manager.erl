%%%-------------------------------------------------------------------
%%% @doc
%%% Configuration module management `gen_server'.
%%%
%%% Handles registration of configuration modules that implement the
%%% `ecc_gen_config' behavior, storage of these configuration modules and
%%% their compiled structs in an ETS table, and attribute access.
%%%
%%% On start, will take in a driver module that implements the `ecc_gen_driver'
%%% behavior and will a) start the driver and b) call the `load/1'
%%% callback. If the driver fails to load configuration values with
%%% `load/1', this server will close and attempt restart according to
%%% the supervisor period/intensity configured for the `ecc' application.
%%%
%%% @see ecc_gen_driver
%%% @see ecc_gen_config
%%% @end
%%%-------------------------------------------------------------------
-module(ecc_config_manager).

-behaviour(gen_server).

-include("ecc_error.hrl").
-include("ecc_config_manager.hrl").

%% API
-export([start_link/1, stop/0, register/1, namespace/1, key/1, table/0]).
%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%%%-------------------------------------------------------------------
%%% @doc
%%% Starts an instance of the ecc_config_manager gen_server. This server
%%% is unique to an erlang node, as it's registered by module name.
%%%
%%% On init, will take in a driver module that implements the `ecc_gen_driver'
%%% behavior and will a) start the driver and b) call the `load/1'
%%% callback. If the driver fails to load configuration values with
%%% `load/1', this server will terminate and return the error reason.
%%%
%%% @see ecc_gen_driver:start_link/1
%%% @see ecc_gen_driver:load/1
%%% @end
%%%-------------------------------------------------------------------
-spec start_link(DriverInit :: ecc_config_manager_init()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(Init = #ecc_config_manager_init{}) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Init, []).

%%%-------------------------------------------------------------------
%%% @doc
%%% Stops the ecc_config_manager gen_server, and the underlying driver
%%% server that implements the `ecc_gen_driver' behavior.
%%% @end
%%%-------------------------------------------------------------------
-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

%%%-------------------------------------------------------------------
%%% @doc
%%% Registers a configuration module with the ecc_config_manager server.
%%% Once registered, `namespace/1' and `key/1' can be called for this
%%% configuration module, based on the module's implementations of the
%%% `load/1' and 'get/2' callbacks.
%%%
%%% Internally, a tuple of the form `{Namespace, Module, ConfigStruct}'
%%% is stored in a protected ETS table:
%%%
%%%     - `Namespace' is equivalent to `Module:namespace/0' and must
%%%       be unique
%%%
%%%     - `ConfigStruct' is equivalent to `Module:load/1'
%%%
%%% @see ecc_config_manager:namespace/1
%%% @see ecc_config_manager:key/1
%%% @end
%%%-------------------------------------------------------------------
-spec register(Req :: ecc_register_module_req()) -> ok | {error, ecc_error()}.
register(Req = #ecc_register_module_req{module = Mod}) when is_atom(Mod) ->
    gen_server:call(?SERVER, Req).

%%%-------------------------------------------------------------------
%%% @doc
%%% Returns the configuration struct for a given registered configuration
%%% module. The input namespace is derived from the target config module's
%%% `namespace/0' callback implementation, from the `ecc_gen_config' behavior.
%%% The return value will be equivalent to the target config module's
%%% `load/1' callback implementation.
%%%
%%% If no config module has been registered with an equivalent namespace,
%%% this function will return undefined.
%%%
%%% @see ecc_config_manager:register/1
%%% @see ecc_gen_config:namespace/0
%%% @see ecc_gen_config:load/1
%%% @end
%%%-------------------------------------------------------------------
-spec namespace(Req :: ecc_get_namespace_req()) -> ecc_config() | undefined.
namespace(Req = #ecc_get_namespace_req{namespace = Ns}) when is_atom(Ns) ->
    gen_server:call(?SERVER, Req).

%%%-------------------------------------------------------------------
%%% @doc
%%% Returns a target attribute of the configuration struct for a given
%%% registered configuration module. The input namespace, `Ns', is derived
%%% from this config module's `namespace/0' callback implementation, from
%%% the `ecc_gen_config' behavior. The return value will be equivalent to
%%% the target config module's `get/2' callback implementation, passing in
%%% `Ns' and `Key'.
%%%
%%% If no config module has been registered with a namespace equivalent to `Ns',
%%% or `get/2' has no implementation for `Key', this function will return
%%% undefined.
%%%
%%% @see ecc_config_manager:register/1
%%% @see ecc_gen_config:namespace/0
%%% @see ecc_gen_config:get/2
%%% @end
%%%-------------------------------------------------------------------
-spec key(Req :: ecc_get_key_req()) -> ecc_config_attribute() | undefined.
key(Req = #ecc_get_key_req{namespace = Ns, key = Key}) when is_atom(Ns), is_atom(Key) ->
    gen_server:call(?SERVER, Req).

%%%-------------------------------------------------------------------
%%% @doc
%%% Returns the underlying ETS table that stores configuration module
%%% tuples, where each module implement the `ecc_gen_config' behavior. The
%%% table is indexed by the namespace returned by the module's `namespace/0'
%%% callback implementation. This table is protected, and thus can only
%%% be read from (and is mostly available for testing purposes).
%%% @end
%%%-------------------------------------------------------------------
-spec table() -> ecc_table().
table() ->
    gen_server:call(?SERVER, #ecc_get_table{}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec init(Args :: term()) ->
    {ok, State :: #ecc_config_manager_state{}}
    | {ok, State :: #ecc_config_manager_state{}, timeout() | hibernate}
    | {stop, Reason :: term()}
    | ignore.
init(Init = #ecc_config_manager_init{}) ->
    case handle_init(Init) of
        {ok, State} ->
            {ok, State};
        {error, Err} ->
            {stop, Err}
    end.

%% @private
%% @doc Handling call messages
-spec handle_call(
    Request :: term(),
    From :: {pid(), Tag :: term()},
    State :: #ecc_config_manager_state{}
) ->
    {reply, Reply :: term(), NewState :: #ecc_config_manager_state{}}
    | {reply, Reply :: term(), NewState :: #ecc_config_manager_state{}, timeout() | hibernate}
    | {noreply, NewState :: #ecc_config_manager_state{}}
    | {noreply, NewState :: #ecc_config_manager_state{}, timeout() | hibernate}
    | {stop, Reason :: term(), Reply :: term(), NewState :: #ecc_config_manager_state{}}
    | {stop, Reason :: term(), NewState :: #ecc_config_manager_state{}}.
handle_call(
    Req = #ecc_register_module_req{},
    _From,
    State = #ecc_config_manager_state{}
) ->
    Reply = handle_register(Req, State),
    {reply, Reply, State};
handle_call(Req = #ecc_get_namespace_req{}, _From, State = #ecc_config_manager_state{}) ->
    Reply = handle_get_namespace(Req, State),
    {reply, Reply, State};
handle_call(Req = #ecc_get_key_req{}, _From, State = #ecc_config_manager_state{}) ->
    Reply = handle_get_key(Req, State),
    {reply, Reply, State};
handle_call(#ecc_get_table{}, _From, State = #ecc_config_manager_state{}) ->
    Reply = handle_get_table(State),
    {reply, Reply, State};
handle_call(_Request, _From, State = #ecc_config_manager_state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling cast messages
-spec handle_cast(Request :: term(), State :: #ecc_config_manager_state{}) ->
    {noreply, NewState :: #ecc_config_manager_state{}}
    | {noreply, NewState :: #ecc_config_manager_state{}, timeout() | hibernate}
    | {stop, Reason :: term(), NewState :: #ecc_config_manager_state{}}.
handle_cast(_Request, State = #ecc_config_manager_state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec handle_info(Info :: timeout() | term(), State :: #ecc_config_manager_state{}) ->
    {noreply, NewState :: #ecc_config_manager_state{}}
    | {noreply, NewState :: #ecc_config_manager_state{}, timeout() | hibernate}
    | {stop, Reason :: term(), NewState :: #ecc_config_manager_state{}}.
handle_info(_Info, State = #ecc_config_manager_state{}) ->
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec terminate(
    Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #ecc_config_manager_state{}
) -> term().
terminate(_Reason, _State = #ecc_config_manager_state{driver_mod = DriverMod, driver = Driver}) ->
    ok = DriverMod:stop(Driver),
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec code_change(
    OldVsn :: term() | {down, term()},
    State :: #ecc_config_manager_state{},
    Extra :: term()
) ->
    {ok, NewState :: #ecc_config_manager_state{}} | {error, Reason :: term()}.
code_change(_OldVsn, State = #ecc_config_manager_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec handle_init(Init :: ecc_config_manager_init()) ->
    {ok, ecc_config_manager_state()} | {error, ecc_error()}.
handle_init(#ecc_config_manager_init{module = DriverMod, args = DriverInit}) ->
    Table = start_table(),
    case DriverMod:start_link(DriverInit) of
        {ok, Driver} ->
            case DriverMod:load(Driver) of
                {ok, Config} ->
                    {ok, #ecc_config_manager_state{
                        driver_mod = DriverMod,
                        driver = Driver,
                        config = Config,
                        config_table = Table
                    }};
                {error, Err} ->
                    EccErr = ecc_error:new(
                        ?ECC_DRIVER_LOAD_ERROR,
                        "failed to load configuration from driver server",
                        Err
                    ),
                    {error, EccErr}
            end;
        {error, Err} ->
            EccErr = ecc_error:new(?ECC_DRIVER_START_ERROR, "failed to start driver server", Err),
            {error, EccErr}
    end.

-spec start_table() -> ecc_table().
start_table() ->
    ets:new(ecc_config_table, [protected, set, {keypos, 2}, {read_concurrency, true}]).

-spec handle_register(Req :: ecc_register_module_req(), State :: ecc_config_manager_state()) ->
    ok | {error, ecc_error()}.
handle_register(#ecc_register_module_req{module = Module}, #ecc_config_manager_state{
    config_table = Table, config = Config
}) ->
    Ns = Module:namespace(),
    case Module:load(Config) of
        {ok, Term} ->
            true = ets:insert(Table, #ecc_config_namespace{
                namespace = Ns, module = Module, config = Term
            }),
            ok;
        {error, Err} ->
            EccErr = ecc_error:new(
                ?ECC_MODULE_LOAD_ERROR, "failed to load configuration module", Err
            ),
            {error, EccErr}
    end.

-spec handle_get_namespace(Req :: ecc_get_namespace_req(), State :: ecc_config_manager_state()) ->
    ecc_config() | undefined.
handle_get_namespace(#ecc_get_namespace_req{namespace = Ns}, #ecc_config_manager_state{
    config_table = Table
}) ->
    case ets:lookup(Table, Ns) of
        [#ecc_config_namespace{namespace = Ns, config = Config}] ->
            Config;
        _ ->
            undefined
    end.

-spec handle_get_key(Req :: ecc_get_key_req(), State :: ecc_config_manager_state()) ->
    ecc_config_attribute() | undefined.
handle_get_key(#ecc_get_key_req{namespace = Ns, key = Key}, #ecc_config_manager_state{
    config_table = Table
}) ->
    case ets:lookup(Table, Ns) of
        [#ecc_config_namespace{namespace = Ns, config = Config, module = Mod}] ->
            Mod:get(Key, Config);
        _ ->
            undefined
    end.

-spec handle_get_table(State :: ecc_config_manager_state()) -> ecc_table().
handle_get_table(#ecc_config_manager_state{config_table = Table}) ->
    Table.
