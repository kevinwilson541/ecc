-module(ecc_test_config).

-behaviour(ecc_gen_config).

%% API
-export([namespace/0, load/1, get/2]).

%%%===================================================================
%%% ecc_gen_config callbacks
%%%===================================================================

namespace() ->
    ?MODULE.

load(#{"BASIC" := BasicVal, "PROCESS_SET_KEY" := SetVal}) ->
    {ok, #{basic => BasicVal, process_set_key => SetVal}};
load(_) ->
    {error, "configuration values BASIC and PROCESS_SET_KEY are not defined"}.

get(basic, #{basic := BasicVal}) ->
    BasicVal;
get(empty, #{process_set_key := SetVal}) ->
    SetVal;
get(_, _) ->
    undefined.
