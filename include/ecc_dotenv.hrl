-define(ECC_DOTENV_LINE_PATTERN,
    % beginning of line
    "(?:^|^)" ++
        % leading space
        "\\s*" ++
        % optional export
        "(?:export\\s+)?" ++
        % key
        "([\\w\\.]+)" ++
        % separator
        "(?:\\s*=\\s*?|:\\s+?)" ++
        % optional value begin
        "(" ++
        % single quoted value
        "\\s*'(?:\'|[^'])*'" ++
        % or
        "|" ++
        % double quoted value
        "\\s*\"(?:\\\"|[^\"])*\"" ++
        % or
        "|" ++
        % unquoted value
        "[^#\\r\\n]+" ++
        % value end
        ")?" ++
        % trailing whitespace
        "\\s*" ++
        % optional comment
        "(?:\\#.*)?" ++
        % end of line
        "(?:$|$)"
).

-define(ECC_DOTENV_QUOTE_PATTERN, "(^['\"])|(['\"]$)").

-type ecc_dotenv_pairs() :: [{string(), string()}].
