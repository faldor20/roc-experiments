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

# isNum : U8 -> Bool
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

PToken : [Close, Num U64, Open, Sep]
PTokenList : List PToken
ParseRes : [ListT (List ParseRes), Num U64]
ParseErr : [InvalidStart PTokenList, MissingAfterStart PTokenList, MissingAfterVal PTokenList]

TokenizerErr : [RunOutt (Bytes-> Result TokenRes TokenizerErr),TokenizeErr]

Bytes:List U8
TokenRes:(PToken,Bytes)


# takeNum: Bytes,Bytes->Result TokenRes (TokenizerErr )
takeNum = \numChars, inp ->
    # dbg numChars
    when inp is
        [] -> Err (RunOutt (\inp2 -> takeNum numChars inp2))
        [num, .. as rest] if isNum num -> takeNum (numChars |> List.append num) rest
        _ -> if numChars |> List.len > 0 then Ok (numChars |> numFromUtf8 |> Num, inp) else Err (TokenizeErr)

# getToken: Bytes-> Result (PToken,Bytes) TokenizerErr
getToken = \input ->
    bytes = eatWhitespace input
    when bytes is
        ['[', .. as rest] -> Ok (Open, rest)
        [']', .. as rest] -> Ok (Close, rest)
        [',', .. as rest] -> Ok (Sep, rest)
        [a, ..] ->
            if isNum a then
                takeNum [] bytes
            else
                Err TokenizeErr

        [] -> Err (RunOutt (getToken))



## KEY idea: there is only two ways, I either need to use a queue/stack data structure or i need recursion. Because I can be parsing a list and then leave that list 

## The reason the last technique uses a token stream is because that allows us to eiminate recursion
## I could make it flat using a stack.
# I should make a stack and use that
pop= \list->
    elem=try (List.last list)
    Ok {rest:List.dropLast list 1,elem:elem}

# push=\list,elem->List.append list elem
empty=[]
# parseGlobalOuter: Bytes->Result ([Continue ,EndOfStream ],(ParseRes),Bytes) ErrList
parseGlobalOuter=\readMore!, input,state->
    # This uses a stack of parents to parse into lists
    # The reason it has this weird wraping is so we can override the getToken function for a single iteration when calling back to a partially parsed token
    # parseGlobal: (Bytes-> Result (PToken,Bytes) TokenizerErr),_,_,_->Result ([Continue ,EndOfStream ],(ParseRes),Bytes) ErrList
    parseGlobal = \thisFetchToken,st, op, inp ->
        {parents,currentList,depth}=st
        when thisFetchToken inp is
        Err (RunOutt (getRest))->
            when readMore! {} is
                End-> Err NoMoreBytes
                Rest bytes2->
                    # dbg (bytes2|>Str.fromUtf8)
                    parseGlobal getRest st op bytes2
        Err _ ->Err TokenizerErr
        Ok (token, bytes) ->
            # dbg token
            # dbg op
            # dbg depth

            when op is
            AfterVal val-> 
                if depth>0 then

                    nextVal=currentList|>List.append val
                    when token is
                    Close-> 
                        #Potentially I should have some global donor list I use to overwrite ther references to this List when I'm no longer going to be accessing it to avoid extra references to my lists
                        if (depth==1) then
                        #End of the last list, this means we should now check whether to continue or not by parsing the next token 
                            parseGlobal
                               getToken
                               {st &depth:0}
                                (AfterVal  (ListT nextVal))
                                bytes

                        # This handles normal list closure
                        else
                            nextList=try (parents|>List.get (depth-1))
                            parseGlobal
                               getToken
                               {st& parents:parents |>List.set depth empty,currentList:nextList,depth:(depth-1)}
                                (AfterVal  (ListT nextVal))
                                bytes
                    Sep -> 
                        parseGlobal 
                            getToken
                            {st&currentList:nextVal}
                            Val
                            bytes
                    t -> Err (MissingAfterVal t)
                else
                    when token is
                    Sep->
                        Ok(val,Continue bytes,state)
                    Close->
                        Ok(val,EndOfStream,state )
                    t -> Err (MissingAfterVal t)
            Val-> 
                when token is 
                Num a-> parseGlobal getToken st (AfterVal(Num a)) bytes
                Open-> 
                    if(depth>0) then 
                    
                        parseGlobal 
                            getToken
                            {parents:parents|>List.set (depth) currentList,currentList:[],depth:depth+1} 
                            Val 
                            bytes
                    else
                        parseGlobal getToken {st&currentList:[],depth:depth+1} Val bytes
                Close-> 
                    #handles the special [] case
                        if (depth==0)then
                        
                            parseGlobal
                               getToken
                               st
                                (AfterVal  (ListT []))
                                bytes
                        else
                            nextList=try (parents|>List.get (depth-1))
                            parseGlobal
                               getToken
                               {st&parents:parents |>List.set depth empty,currentList:nextList,depth:(depth-1)}
                                (AfterVal  (ListT []))
                                bytes

                a-> Err (UnexpectedToken a)

    parseGlobal getToken state Val input 
ParseLoopRes:([Continue ,EndOfStream ],(ParseRes),Bytes)
ErrList:[RunOut (Bytes->Result (ParseLoopRes) ErrList ),TokenizerErr,UnexpectedToken,MissingAfterVal,] 

startParse! : ({}=>[End,Rest Bytes]),Bytes,a,(a,ParseRes->a)=>_
startParse! = \readBytes!, bytesI,stateI,handler ->
    # parseLoop! : (Bytes->Result ParseLoopRes ErrList),_,Bytes =>Result  _ _

    parseLoop = \parseState ,state,bytes->
        # First parse the current value
        when parseGlobalOuter readBytes! bytes parseState is 
        Ok (val, next,parseState2) -> 
            when next is 
            EndOfStream ->
                newState = handler state val
                Ok newState
            Continue remainingBytes->
                newState = handler state val
                parseLoop  parseState2 newState  remainingBytes

        Err a->
            # dbg a 
            Err ParseErr
        # Err _ -> Err Pa
        # Err TokenizeErr->Err ParseErr
        # Err (UnexpectedToken)->Err ParseErr

    parseLoop {parents:List.repeat [] 50,currentList:[],depth:0} stateI bytesI


# valueStream! = \input, readStream!, initState, handle ->
#     loop! = \state, buf ->
#         # dbg state
#         when buf |> tokenize! readStream! is
#             # Hit the final close
#             Ok (tokes, rest, next) ->
#                 parseLoop= \tokens->
#                     # dbg tokens
#                     when parseTokens tokens is
#                         Err e -> Err e
#                         Ok (val, []) ->
#                             nState = handle state val
#                             when (rest, next) is
#                                 (End, NextItem) -> Ok (Done state) # This should maybe error
#                                 (End, Done) -> Ok (Done state)
#                                 (Rest rest2, Done) -> Ok (Done state) # This should maybe error
#                                 (Rest rest2, NextItem) -> Ok (Rest rest2 nState)
#                         Ok (_, restTokens) -> parseLoop restTokens
#                 when parseLoop tokes is
#                 Ok (Rest rest2 nState)->
#                     loop! nState rest2
#                 a->a

#             Err e -> Err e

#     loop! initState input

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
    readBytes! = \{}->
        when reader |> File.read! is
            Err _ -> End
            Ok bytes ->
                when bytes is
                    [] -> End
                    b -> Rest b
    first = try (reader |> File.read! )
    res =try
        startParse! readBytes! first  0 \items, item ->
            # We can imagine doing some kind of effectful handling of this value.
            # Like transforming it and writing it to a file,
            # but for now we will write it to a log, defeating the purpose of streaming ofcourse

            # items |> List.append item
            # dbg "decode"
            items + 1
    try Stdout.line! "$(res|>Inspect.toStr)"
    dbg res
    Ok {}

main! = \_ ->
    try (testParser! {} |> Result.onErr! \a -> Inspect.toStr a |> Stdout.line!)
    try (Stdout.line! "done!")
    Ok {}
