%%%-------------------------------------------------------------------
%%% @doc
%%% Implementation of the ecc_gen_driver behavior module, for loading
%%% configuration values from environment variables and a configurable
%%% dotenv file location. If no dotenv file location is passed, values
%%% will only be read from environment variables.
%%% @end
%%%-------------------------------------------------------------------
-module(ecc_env_driver).

-behaviour(gen_server).
-behaviour(ecc_gen_driver).

-include("ecc_env_driver.hrl").
-include("ecc_env_error.hrl").
-include("ecc_gen_driver.hrl").
-include("ecc_dotenv.hrl").

%% API
-export([
    start_link/0,
    start_link/1,
    stop/1,
    load/1
]).
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

-spec start_link() -> {ok, Pid :: pid()} | {error, Reason :: term()}.
start_link() ->
    start_link(#ecc_env_driver_init{}).

%%%===================================================================
%%% ecc_gen_driver callbacks
%%%===================================================================

-spec start_link(Init :: ecc_env_driver_init()) ->
    {ok, Pid :: pid()} | {error, Reason :: term()}.
start_link(Init = #ecc_env_driver_init{}) ->
    gen_server:start_link(?MODULE, Init, []).

-spec stop(Pid :: pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

-spec load(Pid :: pid()) -> {ok, ecc_gen_driver_load()} | {error, ecc_env_error()}.
load(Pid) ->
    gen_server:call(Pid, #ecc_env_driver_load{}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec init(Args :: term()) ->
    {ok, State :: #ecc_env_driver_state{}}
    | {ok, State :: #ecc_env_driver_state{}, timeout() | hibernate}
    | {stop, Reason :: term()}
    | ignore.
init(#ecc_env_driver_init{dotenv = DotenvFile}) ->
    {ok, #ecc_env_driver_state{dotenv = DotenvFile}}.

%% @private
%% @doc Handling call messages
-spec handle_call(
    Request :: term(),
    From :: {pid(), Tag :: term()},
    State :: #ecc_env_driver_state{}
) ->
    {reply, Reply :: term(), NewState :: #ecc_env_driver_state{}}
    | {reply, Reply :: term(), NewState :: #ecc_env_driver_state{}, timeout() | hibernate}
    | {noreply, NewState :: #ecc_env_driver_state{}}
    | {noreply, NewState :: #ecc_env_driver_state{}, timeout() | hibernate}
    | {stop, Reason :: term(), Reply :: term(), NewState :: #ecc_env_driver_state{}}
    | {stop, Reason :: term(), NewState :: #ecc_env_driver_state{}}.
handle_call(#ecc_env_driver_load{}, _From, State = #ecc_env_driver_state{dotenv = Dotenv}) ->
    Env = os:env(),
    case load_dotenv(Dotenv) of
        {ok, DEnv} ->
            {reply, {ok, maps:from_list(lists:concat([Env, DEnv]))}, State};
        {error, Err} ->
            {reply, {error, Err}, State}
    end;
handle_call(_Request, _From, State = #ecc_env_driver_state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling cast messages
-spec handle_cast(Request :: term(), State :: #ecc_env_driver_state{}) ->
    {noreply, NewState :: #ecc_env_driver_state{}}
    | {noreply, NewState :: #ecc_env_driver_state{}, timeout() | hibernate}
    | {stop, Reason :: term(), NewState :: #ecc_env_driver_state{}}.
handle_cast(_Request, State = #ecc_env_driver_state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec handle_info(Info :: timeout() | term(), State :: #ecc_env_driver_state{}) ->
    {noreply, NewState :: #ecc_env_driver_state{}}
    | {noreply, NewState :: #ecc_env_driver_state{}, timeout() | hibernate}
    | {stop, Reason :: term(), NewState :: #ecc_env_driver_state{}}.
handle_info(_Info, State = #ecc_env_driver_state{}) ->
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec terminate(
    Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #ecc_env_driver_state{}
) -> term().
terminate(_Reason, _State = #ecc_env_driver_state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec code_change(
    OldVsn :: term() | {down, term()},
    State :: #ecc_env_driver_state{},
    Extra :: term()
) ->
    {ok, NewState :: #ecc_env_driver_state{}} | {error, Reason :: term()}.
code_change(_OldVsn, State = #ecc_env_driver_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec load_dotenv(FileName :: string() | undefined) ->
    {ok, ecc_dotenv_pairs()} | {error, ecc_env_error()}.
load_dotenv(undefined) ->
    {ok, []};
load_dotenv(File) ->
    case file:read_file(File) of
        {ok, Bin} ->
            {ok, ecc_dotenv:parse(Bin)};
        {error, Err} ->
            {error,
                ecc_env_error:new(
                    ?ECC_ENV_READ_ERROR, "failed to read the configured .env file", Err
                )}
    end.
