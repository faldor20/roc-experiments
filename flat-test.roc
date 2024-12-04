app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
}

import pf.Stdout
# THis will be a test of decoding a flat version of json. This version will contain only arrays and numbers.
main =
    res =
        "[1,2,[ 2 ,3 ]],[1,3]]"
        |> Str.toUtf8
        # |>valueStream [] \state,val-> state|>List.append val
        |> tokenize
        |> Result.try \(tokens, _) ->
            dbg tokens
            tokens |> parseTokens

    out =
        when res is
            Ok a -> []
            Err _ -> [ListT []]
    dbg res
    Stdout.line! "hi"

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

# My first approach tried to keep the current state of the system in memory at the top level and was a total failure. I cannot even type check it because the addToWips function needs to have a recursive type it's a mess I hate it. THe only solution would be to wrap each thing in a tag which is just building a token stream which is my next approach

# findNext = \state, rest ->
#     when rest is
#         '[' -> Array []
#         _ -> Other

# addToWips =\wips, indexes, item ->
#     # when List is
#     when indexes is
#         [] -> wips |> List.append item
#         [idx, .. as rest] ->
#             wips
#             |> List.update idx \inner ->
#                 addToWips inner rest item

# Imagine we are parsing [1,[[[1,2,4,5,[1,|2]]]]]
# Now even simpler[1[2,3]]
# parse = \input ->
#     loop = \state, inp ->
#         when { wipOverall, indexes, currentState } is
#             Num list ->
#                 when inp is
#                     [] -> Err { indexes, current }
#                     [n, .. as rest] if isNum n -> loop (Num n)
#                     [',', .. as rest] ->
#                         findNext { wips } rest

#             # [a,..] -> isNum n-> loop (Num n)
#             Array list -> {}

#     loop input

numFromUtf8 = \nums ->
    nums
    |> List.walkBackwards (0, 1) \(num, idx), numChar ->
        (((numChar - 0x30) * idx) + num, idx + 1)
    |> .0

# token stream
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

# This is correct but doesn't work becasue we need macros/compiler magic to make it type check
# parse=\last,state,tokens->
#     when last is
#     Val->
#         when tokens is
#         [Sep,.. as rest2]-> parse Sep state rest2
#         _-> Err MissingSep
#     ListStart->
#         when tokens is
#         [Close,.. as rest]-> Ok (state,rest )
#         [Num num,.. as rest]-> parse Val (state|>List.append  num) rest
#         [ListStart,.. as rest] ->
#             when (parse ListStart [] rest)is
#             Err e->Err e
#             Ok (val,rest2)->
#                 parse Val (state|>List.append val  ) rest2
#         _-> Err MissingSep
#     Sep->
#         when tokens is
#         [Sep,.. as rest2]-> (parse Sep state rest2)
#         _-> Err MissingSep

# PTokenRes:[MissingSep,MissingStart,MissingVal]
PToken : [Close, Num U8, Open, Sep]
ParseRes : [ListT (List ParseRes), Num U8]

parseTokens : List PToken -> Result ParseRes _
parseTokens = \tokenList ->
    parse : _, List ParseRes, List PToken -> Result (ParseRes, List PToken) _
    parse = \last, state, tokens ->
        dbg state
        dbg tokens
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
    |> Result.map \a ->

        dbg a.0
        a.0

# valueStream= \input,initState, handle ->
#     loop=\state,inp->
#         when inp|>tokenize is
#             Ok (tokens,rest)->
#                 when parseTokens tokens  is
#                 Err e->Err e
#                 Ok(_,[_,..])-> Err LeftoverTokens
#                 Ok (val,[])->
#                     nState=handle state val
#                     when rest is
#                         End ->Ok state
#                         Rest rest2->  loop nState rest2
#             Err e->Err e

#     loop initState input

