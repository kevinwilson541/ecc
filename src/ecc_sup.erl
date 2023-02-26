%%%-------------------------------------------------------------------
%%% @doc
%%% Module that implements the top-level supervisor for the `ecc' application.
%%% @end
%%%-------------------------------------------------------------------

-module(ecc_sup).

-behaviour(supervisor).

-include("ecc_config.hrl").
-include("ecc_sup.hrl").
-include("ecc_config_manager.hrl").
-include("ecc_env_driver.hrl").

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% supervisor callbacks
%%%===================================================================

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupIntensity = application:get_env(ecc, ?ECC_SUP_INTENSITY, ?ECC_SUP_DEFAULT_INTENSITY),
    SupPeriod = application:get_env(ecc, ?ECC_SUP_PERIOD, ?ECC_SUP_DEFAULT_PERIOD),

    SupFlags = #{
        strategy => one_for_all,
        intensity => SupIntensity,
        period => SupPeriod
    },

    DriverMod = application:get_env(ecc, ?ECC_DRIVER_MODULE, ?ECC_DRIVER_DEFAULT_MODULE),
    DriverArgs = application:get_env(ecc, ?ECC_DRIVER_ARGUMENTS, #ecc_env_driver_init{}),
    DriverShutdown = application:get_env(ecc, ?ECC_DRIVER_SHUTDOWN, ?ECC_DRIVER_DEFAULT_SHUTDOWN),

    ChildSpecs = [
        #{
            id => ecc_config_manager,
            start =>
                {ecc_config_manager, start_link, [
                    #ecc_config_manager_init{module = DriverMod, args = DriverArgs}
                ]},
            restart => permanent,
            shutdown => DriverShutdown,
            type => worker,
            modules => [ecc_config_manager, DriverMod]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
