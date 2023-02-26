%%%-------------------------------------------------------------------
%%% @doc
%%% Module that implements dotenv parsing of binary strings. Internally,
%%% this module receives input from a call to `file:read_file/1', which
%%% reads a configured path for a dotenv file.
%%%
%%% @reference https://github.com/motdotla/dotenv
%%% @reference https://github.com/bkeepers/dotenv
%%% @end
%%%-------------------------------------------------------------------
-module(ecc_dotenv).

-include("ecc_dotenv.hrl").

-export([parse/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%%-------------------------------------------------------------------
%%% @doc
%%% Parses an input binary string into a list of key/value pairs, using
%%% the dotenv file format.
%%%
%%% @param Bin  Input binary string for parsing.
%%% @returns `ecc_dotenv_pairs()'
%%% @end
%%%-------------------------------------------------------------------
-spec parse(Bin :: binary()) -> ecc_dotenv_pairs().
parse(Bin) when is_binary(Bin) ->
    % normalize newlines
    OnlyNewLine = binary:replace(Bin, <<"\r\n">>, <<"\n">>, [global]),
    % split into lines for regexp parsing
    Lines = binary:split(OnlyNewLine, <<"\n">>, [global]),
    % parse dotenv file lines
    % there's no proplists:new, so we hac
    parse_lines(Lines, new_proplist()).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec new_proplist() -> ecc_dotenv_pairs().
new_proplist() ->
    maps:to_list(maps:new()).

-spec parse_lines(Lines :: [binary()], Pairs :: ecc_dotenv_pairs()) -> ecc_dotenv_pairs().
parse_lines([], Pairs) ->
    Pairs;
parse_lines([Line | Rest], Pairs) ->
    case re:run(Line, ?ECC_DOTENV_LINE_PATTERN, [global, {capture, all, list}]) of
        {match, Match} ->
            MatchPairs = create_pairs_from_match(Match, []),
            NPairs = lists:concat([MatchPairs, Pairs]),
            parse_lines(Rest, NPairs);
        nomatch ->
            parse_lines(Rest, Pairs)
    end.

-spec create_pairs_from_match(Matches :: list(), Pairs :: ecc_dotenv_pairs()) -> ecc_dotenv_pairs().
create_pairs_from_match([], Pairs) ->
    Pairs;
create_pairs_from_match([[_, Key, Val] | Rest], Pairs) ->
    create_pairs_from_match(Rest, [{Key, format_value(Val)} | Pairs]);
create_pairs_from_match([[_, Key] | Rest], Pairs) ->
    create_pairs_from_match(Rest, [{Key, ""} | Pairs]).

-spec format_value(Val :: string()) -> string().
format_value(Val) ->
    % remove leading and trailing spaces from value
    Trimmed = string:trim(Val),
    % remove surrounding quotes (single and double)
    re:replace(Trimmed, ?ECC_DOTENV_QUOTE_PATTERN, "", [global, {return, list}]).
