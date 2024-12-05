app [main!] {
    pf: platform "../basic-cli/platform/main.roc",
}

import pf.Stdout
import pf.File
# THis will be a test of decoding a flat version of json. This version will contain only arrays and numbers.

# =============
#   Tokenizer
# =============

isWhitespace = \b ->
    when b is
        ' ' | '\n' | '\r' | '\t' -> Bool.true
        _ -> Bool.false
eatWhitespace = \bytes ->
    when bytes is
        [a, ..] if isWhitespace a -> eatWhitespace (List.dropFirst bytes 1)
        _ -> bytes
isNum : U8 -> Bool
isNum = \char -> char >= '0' && char <= '9'

numFromUtf8 = \nums ->
    res =
        nums
        |> List.walkBackwards (0, 1) \(num, idx), numChar ->
            (((((numChar - 0x30) |> Num.toU64) * idx) + num), idx * 10)
        |> .0
    res

expect
    ['2', '8'] |> numFromUtf8 == 28
expect
    ['2', '8', '8', '0'] |> numFromUtf8 == 2880
expect
    ['2', '0', '8', '0'] |> numFromUtf8 == 2080
# =============
#   Tokenizer
# =============

# We use a continue function in the tokenizer so we can pause at any point

takeNum = \numChars, inp ->
    when inp is
        [] -> Err (RunOutt (\inp2 -> takeNum numChars inp2))
        [num, .. as rest] if isNum num -> takeNum (numChars |> List.append num) rest
        _ -> if numChars |> List.len > 0 then Ok (numChars |> numFromUtf8 |> Num, inp) else Err (TokenizeErr)

# getToken: _-> Result _ _
getToken = \input ->
    when input is
        ['[', .. as rest] -> Ok (Open, rest)
        [']', .. as rest] -> Ok (Close, rest)
        [',', .. as rest] -> Ok (Sep, rest)
        [a, ..] ->
            if isNum a then
                takeNum [] input
            else
                Err TokenizeErr

        [] -> Err (RunOutt (getToken))

tokenizeLoop = \{ tokens, continue, depth, lastToken }, inp ->
    when inp |> eatWhitespace |> continue is
        Err (RunOutt continueState) ->
            Err (RunOut { continue: continueState, tokens, depth, lastToken })

        Ok (token, rest) ->
            if lastToken then
                when token is
                    Sep ->
                        Ok (tokens, rest, NextItem)

                    Close ->
                        Ok (tokens, rest, Done)

                    _ -> Err TokenizeErr
            else
                newDepth =
                    when token is
                        Open -> depth + 1
                        Close -> depth - 1
                        _ -> depth
                newTokens = (tokens |> List.append token)
                { tokens: newTokens, continue: \inp2 -> getToken inp2, depth: newDepth, lastToken: newDepth == 0 }
                |> tokenizeLoop rest

        Err TokenizeErr -> Err (TokenizeErr)

## Pretend this function is effectful.
# `readBytes` would be an effectful function that returns more bytes
tokenize! = \input, readBytes! ->
    # read from the stream until we run out, read from the byte stream and then read again is needed
    loop! = \state, buf ->
        when state |> tokenizeLoop buf is
            Err TokenizeErr -> Err TokenizeErr
            Err (RunOut ranOutState) ->
                when readBytes! buf is
                    Rest rest -> ranOutState |> loop! rest
                    End -> Ok (ranOutState.tokens, End, Done)

            Ok (tokens, rest, next) ->
                Ok (tokens, Rest rest, next)
    loop! { tokens: [], continue: \inp2 -> getToken inp2, depth: 0, lastToken: Bool.false } input

expect
    tokenize! ("[12,122,[300]]" |> Str.toUtf8) \_ -> End
    |> Result.isOk
# Shows that we can resume tokenizing within a number
expect
    res = tokenize!
        ("[12,122,[30" |> Str.toUtf8)
        \_ -> "0]]" |> Str.toUtf8 |> Rest
    res
    |> Result.isOk

# =============
#   Parser
# =============

PToken : [Close, Num U64, Open, Sep]
PTokenList : List PToken
ParseRes : [ListT (List ParseRes), Num U64]
ParseErr : [InvalidStart PTokenList, MissingAfterStart PTokenList, MissingAfterVal PTokenList]

parseTokens : List PToken -> Result (ParseRes, PTokenList) _
parseTokens = \tokenList ->
    parse : _, List ParseRes, List PToken -> Result (ParseRes, List PToken) ParseErr
    parse = \last, state, tokens ->
        when last is
            Val ->
                when tokens is
                    [Sep, .. as rest2] -> parse Sep state rest2
                    [Close, .. as rest] -> Ok (ListT state, rest)
                    _ -> Err (MissingAfterVal tokens)

            ListStart | Sep ->
                when tokens is
                    [Close, .. as rest] -> Ok (ListT state, rest)
                    [Num num, .. as rest] -> parse Val (state |> List.append (Num num)) rest
                    [Open, .. as rest] ->
                        when parse ListStart [] rest is
                            Err e -> Err e
                            Ok (val, rest2) ->
                                parse Val (state |> List.append (val)) rest2

                    _ -> Err (MissingAfterStart tokens)

            Start ->
                when tokens is
                    [Num num, .. as rest] -> Ok (Num num, rest)
                    [Open, .. as rest] ->
                        when parse ListStart [] rest is
                            Err e -> Err e
                            Ok (val, rest2) ->
                                Ok (val, rest2)

                    _ -> Err (InvalidStart tokens)

    parse Start [] tokenList

valueStream! = \input, readStream!, initState, handle ->
    loop! = \state, buf ->
        when buf |> tokenize! readStream! is
            # Hit the final close
            Ok (tokens, rest, next) ->
                # dbg tokens
                when parseTokens tokens is
                    Err e -> Err e
                    Ok (_, [_, ..]) -> Err LeftoverTokens
                    Ok (val, []) ->
                        nState = handle state val
                        when (rest, next) is
                            (End, NextItem) -> Ok state # This should maybe error
                            (End, Done) -> Ok state
                            (Rest rest2, Done) -> Ok state # This should maybe error
                            (Rest rest2, NextItem) -> loop! nState rest2

            Err e -> Err e

    loop! initState input

# =============
#   Invocation
# =============

testParser! = \_ ->
    input =
        "[1,2,[ 2 ,3 ]],[1,3]]"
        |> Str.toUtf8

    try (Stdout.line! ("starting"))
    len = 10000
    buf = List.repeat 0u8 len
    reader = try (File.openReaderWithBuf! "input.txt" (buf))
    readBytes! = \buffer ->
        when reader |> File.readBytesBuf! buffer is
            Err _ -> End
            Ok bytes ->
                when bytes is
                    [] -> End
                    b -> Rest b
    first = try (reader |> File.readBytesBuf! buf)
    res =
        first
        |> valueStream!? readBytes! 0 \items, item ->
            # We can imagine doing some kind of effectful handling of this value.
            # Like transforming it and writing it to a file,
            # but for now we will write it to a log, defeating the purpose of streaming ofcourse

            # items |> List.append item
            # dbg "decode"
            items + 1
    dbg res
    Ok {}

main! = \_ ->
    try (testParser! {} |> Result.onErr! \a -> Inspect.toStr a |> Stdout.line!)
    try Stdout.line! "done!"
    Ok {}
