app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
}

import pf.Stdout
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
    nums
    |> List.walkBackwards (0, 1) \(num, idx), numChar ->
        (((numChar - 0x30) * idx) + num, idx + 1)
    |> .0

# =============
#   Tokenizer
# =============

takeNum = \numChars, inp ->
    when inp is
        [] -> Err (RunOut (\inp2 -> takeNum numChars inp2))
        [num, .. as rest] if isNum num -> takeNum (numChars |> List.append num) rest
        _ -> if numChars |> List.len > 0 then Ok (numChars |> numFromUtf8 |> Num, inp) else Err NotNum

getToken = \input ->
    when input is
        ['[', .. as rest] -> Ok (Open, rest)
        [']', .. as rest] -> Ok (Close, rest)
        [',', .. as rest] -> Ok (Sep, rest)
        [a, ..] ->
            if isNum a then
                takeNum [] input
            else
                Err Unknown

        [] -> Err (RunOut (getToken))

readStream = \{} -> End
tokenize = \input ->
    loop = \tokens, state, depth, inp ->
        when inp |> eatWhitespace |> state is
            Err (RunOut continueState) ->
                when readStream {} is
                    Next bytes ->
                        loop tokens continueState depth bytes

                    End -> Ok (tokens, End)

            Ok (token, rest) ->
                newDepth =
                    when token is
                        Open -> depth + 1
                        Close -> depth - 1
                        _ -> depth
                newTokens = (tokens |> List.append token)
                if newDepth == 0 then
                    Ok (newTokens, Rest rest)
                else
                    loop newTokens (\inp2 -> getToken inp2) newDepth rest

            Err NotNum -> Err NotNum
            Err Unknown -> Err Unknown

    loop [] (\inp2 -> getToken inp2) 0 input

# =============
#   Parser
# =============

PToken : [Close, Num U8, Open, Sep]
PTokenList : List PToken
ParseRes : [ListT (List ParseRes), Num U8]
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

valueStream = \input, initState, handle ->
    loop = \state, inp ->
        when inp |> tokenize is
            # Hit the final close
            Ok ([Close], End) -> Ok state
            # Just found a seperator token this means we are betwen top level items. This is a hack and I should probably parse it along with the item
            Ok ([Sep], rest) ->
                when rest is
                    End -> Ok state
                    Rest rest2 -> loop state rest2

            Ok (tokens, rest) ->
                dbg tokens
                when parseTokens tokens is
                    Err e -> Err e
                    Ok (_, [_, ..]) -> Err LeftoverTokens
                    Ok (val, []) ->
                        nState = handle state val
                        when rest is
                            End -> Ok state
                            Rest rest2 -> loop nState rest2

            Err e -> Err e

    loop initState input

# =============
#   Invocation
# =============

main =
    input =
        "[1,2,[ 2 ,3 ]],[1,3]]"
        |> Str.toUtf8

    res2 =
        input
        |> valueStream [] \items, item ->
            # We can imagine doing some kind of effectful handling of this value. 
            #Like transforming it and writing it to a file,
            #but for now we will write it to a log, defeating the purpose of streaming ofcourse
            items |> List.append item

    # dbg res
    dbg res2
    Stdout.line! "hi"