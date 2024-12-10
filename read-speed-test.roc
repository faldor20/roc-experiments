
app [main!] {
    pf: platform "../basic-cli/platform/main.roc",
}

import pf.Stdout
import pf.File

testRawReadSpeed! = \{} ->
    len = 8000
    buf = List.range { start: At 0, end: Before len } |> List.map \a -> 0u8
    reader = try (File.openReaderWithBuf! "input.txt" (buf))
    otherBuf = []

    loop! = \state, buf2 ->
        readRes = reader |> File.read! 
        when readRes is
            Err e ->
                Err (e, state)

            Ok [] ->
                Ok state

            Ok res ->
                loop!
                    (state + (res |> List.len))
                    buf2

    # !! pass otherBuf in here to make it create a new buf each iteration
    done = loop! 0 buf

    Stdout.line! "done!!$(Inspect.toStr (done))"

main! = \_ ->
    try (testRawReadSpeed! {} |> Result.onErr! \a -> Inspect.toStr a |> Stdout.line!)
    try Stdout.line! "done!"
    Ok {}
