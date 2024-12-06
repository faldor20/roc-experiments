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
        [a, ..as rest ] if isWhitespace a -> eatWhitespace rest
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

takeNum! = \numChars, inp, readMore! ->
    # dbg (Str.fromUtf8 numChars)
    when inp is
        [] ->
            when try readMore! inp is
                [] -> Err (EndOfData)
                a -> takeNum! numChars a readMore!

        [num, .. as rest] if isNum num -> takeNum! (numChars |> List.append num) rest readMore!
        _ -> if numChars |> List.len > 0 then Ok (numChars |> numFromUtf8 |> Num, inp) else Err (TokenizeErr (FailedParsingNum))

# getToken: _-> Result _ _
getToken! = \input, readMore! ->
    bytes = eatWhitespace input
    when bytes is
        ['[', .. as rest] -> Ok (Open, rest)
        [']', .. as rest] -> Ok (Close, rest)
        [',', .. as rest] -> Ok (Sep, rest)
        [a, .. as rest] ->
            if isNum a then
                takeNum! [a] rest readMore!
            else
                Err (TokenizeErr MissingNum)

        [] as out ->
            when try readMore! out is
                [] -> Err (EndOfData)
                a -> getToken! a readMore!

parseList = \items, inp, readMore! ->
    (token, bytes) = try getToken! inp readMore!
    # dbg (bytes |> Str.fromUtf8)
    # dbg "list token  $(Inspect.toStr token)"
    when token is
        Num n ->
            nextItems = items |> List.append (NumT n)
            (token1, bytes1) = try getToken! bytes readMore!
            when token1 is
                Close -> Ok (ListT nextItems, bytes1)
                Sep -> parseList nextItems bytes1 readMore!
                _ -> Err MissingAfterVal

        Open ->
            # dbg "list inside list"
            (Y, bytes1) = try parseList items bytes readMore!
            nextItems = items |> List.append (inner)
            # dbg nextItems
            (token2, bytes2) = try getToken! bytes1 readMore!
            # dbg token2
            when token2 is
                Close -> Ok (ListT nextItems, bytes2)
                Sep -> parseList nextItems bytes2 readMore!
                _ -> Err MissingAfterVal

        Close -> Ok (ListT [], bytes)
        _ -> Err (InvalidStart ("Invalid start:  $(Inspect.toStr token)"))

parse2 = \inp, readMore! ->
    (token, bytes) = try getToken! inp readMore!
    # dbg token
    when token is
        Num n -> Ok (NumT n, bytes)
        Open ->
            (result, bytes1) = try parseList [] bytes readMore!
            Ok (ListT result, bytes1)

        _ -> Err (InvalidNextToken ("Invalid start:  $(Inspect.toStr token)"))

parseLoop = \handler, state, bytes, readMore! ->
    # First parse the current value
    (val, remainingBytes) = try parse2 bytes readMore!

    # Update state with handler
    newState = handler state val
    # dbg (bytes|>List.len)

    # Get next token to determine if we continue
    (token, nextBytes) = try getToken! remainingBytes readMore!
    when token is
        Sep -> parseLoop handler newState nextBytes readMore!
        Close -> Ok newState
        t -> Err (InvalidNextToken "Invalid token after parse $(Inspect.toStr t)")

startParse = \handler, state, readMore! ->
    \bytes -> parseLoop handler state bytes readMore!

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
            Err _ -> Err (EndOfData)
            Ok bytes -> Ok bytes

    first = try (reader |> File.readBytesBuf! buf)

    res = try parseLoop (\handlerState, _ ->
     # dbg handlerState
     handlerState+ 1) 1 first readBytes!

    try Stdout.line! "$(res|>Inspect.toStr)"
    Ok {}

main! = \_ ->
    try (testParser! {} |> Result.onErr! \a -> Inspect.toStr a |> Stdout.line!)
    try Stdout.line! "done!"
    Ok {}
