-module(ecc_dotenv_tests).

-include("ecc_dotenv.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% Test suites

basic_test_() ->
    {
        setup,
        fun start/0,
        fun basic_test_run/1
    }.

after_line_test_() ->
    {
        setup,
        fun start/0,
        fun after_line_test_run/1
    }.

empty_test_() ->
    {
        setup,
        fun start/0,
        fun empty_test_run/1
    }.

empty_single_quotes_test_() ->
    {
        setup,
        fun start/0,
        fun empty_single_quotes_test_run/1
    }.

empty_double_quotes_test_() ->
    {
        setup,
        fun start/0,
        fun empty_double_quotes_test_run/1
    }.

single_quotes_test_() ->
    {
        setup,
        fun start/0,
        fun single_quotes_test_run/1
    }.

single_quotes_spaced_test_() ->
    {
        setup,
        fun start/0,
        fun single_quotes_spaced_test_run/1
    }.

double_quotes_test_() ->
    {
        setup,
        fun start/0,
        fun double_quotes_test_run/1
    }.

double_quotes_spaced_test_() ->
    {
        setup,
        fun start/0,
        fun double_quotes_spaced_test_run/1
    }.

double_quotes_inside_single_test_() ->
    {
        setup,
        fun start/0,
        fun double_quotes_inside_single_test_run/1
    }.

double_quotes_with_no_space_bracket_test_() ->
    {
        setup,
        fun start/0,
        fun double_quotes_with_no_space_bracket_test_run/1
    }.

single_quotes_inside_double_test_() ->
    {
        setup,
        fun start/0,
        fun single_quotes_inside_double_test_run/1
    }.

expand_newlines_test_() ->
    {
        setup,
        fun start/0,
        fun expand_newlines_test_run/1
    }.

dont_expand_unquoted_test_() ->
    {
        setup,
        fun start/0,
        fun dont_expand_unquoted_test_run/1
    }.

dont_expand_squoted_test_() ->
    {
        setup,
        fun start/0,
        fun dont_expand_squoted_test_run/1
    }.

inline_comments_test_() ->
    {
        setup,
        fun start/0,
        fun inline_comments_test_run/1
    }.

inline_comments_single_quotes_test_() ->
    {
        setup,
        fun start/0,
        fun inline_comments_single_quotes_test_run/1
    }.

inline_comments_double_quotes_test_() ->
    {
        setup,
        fun start/0,
        fun inline_comments_double_quotes_test_run/1
    }.

inline_comments_space_test_() ->
    {
        setup,
        fun start/0,
        fun inline_comments_space_test_run/1
    }.

equal_signs_test_() ->
    {
        setup,
        fun start/0,
        fun equal_signs_test_run/1
    }.

retain_inner_quotes_test_() ->
    {
        setup,
        fun start/0,
        fun retain_inner_quotes_test_run/1
    }.

retain_inner_quotes_as_string_test_() ->
    {
        setup,
        fun start/0,
        fun retain_inner_quotes_as_string_test_run/1
    }.

trim_space_from_unquoted_test_() ->
    {
        setup,
        fun start/0,
        fun trim_space_from_unquoted_test_run/1
    }.

%%% Test hooks

start() ->
    {ok, CWD} = file:get_cwd(),
    Path = filename:join([CWD, "test", ".env.test"]),
    {ok, File} = file:read_file(Path),
    ecc_dotenv:parse(File).

%%% Test runners

basic_test_run(Pairs) ->
    Basic = proplists:get_value("BASIC", Pairs),
    ?_assertEqual(Basic, "basic").

after_line_test_run(Pairs) ->
    AfterLine = proplists:get_value("AFTER_LINE", Pairs),
    ?_assertEqual(AfterLine, "after_line").

empty_test_run(Pairs) ->
    Empty = proplists:get_value("EMPTY", Pairs),
    ?_assertEqual(Empty, "").

empty_single_quotes_test_run(Pairs) ->
    Value = proplists:get_value("EMPTY_SINGLE_QUOTES", Pairs),
    ?_assertEqual(Value, "").

empty_double_quotes_test_run(Pairs) ->
    Value = proplists:get_value("EMPTY_DOUBLE_QUOTES", Pairs),
    ?_assertEqual(Value, "").

single_quotes_test_run(Pairs) ->
    Value = proplists:get_value("SINGLE_QUOTES", Pairs),
    ?_assertEqual(Value, "single_quotes").

single_quotes_spaced_test_run(Pairs) ->
    Value = proplists:get_value("SINGLE_QUOTES_SPACED", Pairs),
    ?_assertEqual(Value, "    single quotes    ").

double_quotes_test_run(Pairs) ->
    Value = proplists:get_value("DOUBLE_QUOTES", Pairs),
    ?_assertEqual(Value, "double_quotes").

double_quotes_spaced_test_run(Pairs) ->
    Value = proplists:get_value("DOUBLE_QUOTES_SPACED", Pairs),
    ?_assertEqual(Value, "    double quotes    ").

double_quotes_inside_single_test_run(Pairs) ->
    Value = proplists:get_value("DOUBLE_QUOTES_INSIDE_SINGLE", Pairs),
    ?_assertEqual(Value, "double \"quotes\" work inside single quotes").

double_quotes_with_no_space_bracket_test_run(Pairs) ->
    Value = proplists:get_value("DOUBLE_QUOTES_WITH_NO_SPACE_BRACKET", Pairs),
    ?_assertEqual(Value, "{ port: $MONGOLAB_PORT}").

single_quotes_inside_double_test_run(Pairs) ->
    Value = proplists:get_value("SINGLE_QUOTES_INSIDE_DOUBLE", Pairs),
    ?_assertEqual(Value, "single 'quotes' work inside double quotes").

expand_newlines_test_run(Pairs) ->
    Value = proplists:get_value("EXPAND_NEWLINES", Pairs),
    ?_assertEqual(Value, "expand\\nnew\\nlines").

dont_expand_unquoted_test_run(Pairs) ->
    Value = proplists:get_value("DONT_EXPAND_UNQUOTED", Pairs),
    ?_assertEqual(Value, "dontexpand\\nnewlines").

dont_expand_squoted_test_run(Pairs) ->
    Value = proplists:get_value("DONT_EXPAND_SQUOTED", Pairs),
    ?_assertEqual(Value, "dontexpand\\nnewlines").

inline_comments_test_run(Pairs) ->
    Value = proplists:get_value("INLINE_COMMENTS", Pairs),
    ?_assertEqual(Value, "inline comments").

inline_comments_single_quotes_test_run(Pairs) ->
    Value = proplists:get_value("INLINE_COMMENTS_SINGLE_QUOTES", Pairs),
    ?_assertEqual(Value, "inline comments outside of #singlequotes").

inline_comments_double_quotes_test_run(Pairs) ->
    Value = proplists:get_value("INLINE_COMMENTS_DOUBLE_QUOTES", Pairs),
    ?_assertEqual(Value, "inline comments outside of #doublequotes").

inline_comments_space_test_run(Pairs) ->
    Value = proplists:get_value("INLINE_COMMENTS_SPACE", Pairs),
    ?_assertEqual(Value, "inline comments start with a").

equal_signs_test_run(Pairs) ->
    Value = proplists:get_value("EQUAL_SIGNS", Pairs),
    ?_assertEqual(Value, "equals==").

retain_inner_quotes_test_run(Pairs) ->
    Value = proplists:get_value("RETAIN_INNER_QUOTES", Pairs),
    ?_assertEqual(Value, "{\"foo\": \"bar\"}").

retain_inner_quotes_as_string_test_run(Pairs) ->
    Value = proplists:get_value("RETAIN_INNER_QUOTES_AS_STRING", Pairs),
    ?_assertEqual(Value, "{\"foo\": \"bar\"}").

trim_space_from_unquoted_test_run(Pairs) ->
    Value = proplists:get_value("TRIM_SPACE_FROM_UNQUOTED", Pairs),
    ?_assertEqual(Value, "some spaced out string").
