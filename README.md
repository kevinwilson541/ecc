ecc
=====

An OTP application for compiling configuration values into well-structured and validated namespaces.

Configuration values can be loaded via an implementation of the [`ecc_gen_driver`](./src/ecc_gen_driver.erl) behavior
module. By default, a driver is supplied for loading configuration values via environment variables and an optional
dotenv file path. This driver can be found in the [`ecc_env_driver`](./src/ecc_env_driver.erl) module.

Validation and namespacing of configuration values occurs through modules that implement
the [`ecc_gen_config`](./src/ecc_gen_config.erl) behavior module. Each configuration module exports a set of functions
that:

- declare the namespace its configuration will be stored under (this must be unique), via `namespace/0`
- transform the configuration set (loaded by a `ecc_gen_driver` module) into a validated structure, via `load/1`
- provide a means to access attributes of this validated structure, via `get/2`

An example implementation of `ecc_gen_config` can be found in the test suite,
under [`ecc_test_config.erl`](./test/ecc_test_config.erl).


Build
-----

    $ rebar3 compile

Test
-----

    $ rebar3 eunit

Documentation
-----

    $ rebar3 edoc

Formatting
-----

    $ rebar3 fmt

Usage
-----

Example configuration module implementation:

```erlang
-module(example_config_module).

-behavior(ecc_gen_config).

-export([namespace/0, load/1, get/2]).

-record(example_config_rec, {
    foo :: string(),
    bar :: integer(),
    baz :: boolean()
}).

%%%===================================================================
%%% ecc_gen_config callbacks
%%%===================================================================

namespace() ->
    ?MODULE.

load(Maps = #{"FOO" := Foo, "BAR" := Bar}) ->
    % or handle this as a result, for simplicity of this example we won't provide a helpful error message
    BarInt = parse_int(Bar),
    % or handle this as a result, for simplicity of this example we won't provide a helpful error message
    BazBool = parse_bool(maps:get("BAZ", Maps, "false")),
    {ok, #example_config_rec{foo = Foo, bar = BarInt, baz = BazBool}};
load(#{"FOO" := _Foo}) ->
    {error, "missing required FOO value"};
load(#{"BAR" := _Bar}) ->
    {error, "missing required BAR value"};
load(_) ->
    {error, "missing required FOO and BAR values"}.

get(foo, #example_config_rec{foo = Foo}) ->
    Foo;
get(bar, #example_config_rec{bar = Bar}) ->
    Bar;
get(baz, #example_config_rec{baz = Baz}) ->
    Baz;
get(_, _) ->
    undefined.

%%%===================================================================
%%% Internal functions
%%%===================================================================

parse_int(Val) when is_list(Val) ->
    erlang:list_to_integer(Val).

parse_bool("true") ->
    true;
parse_bool("false") ->
    false.
```

Then to register this module with the `ecc` application:

```erlang
ok = ecc:register(example_config_module)
```

Subsequent usage, once the configuration module has been registered:

```erlang
#example_config_rec{foo = Foo} = ecc:namespace(example_config_module),
Foo = ecc:key(example_config_module, foo)
```