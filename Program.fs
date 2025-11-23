open BFInterpreter.BF
open BFInterpreter.REPL

let testCases =
    seq {
        ("-[--->+<]>-.[---->+++++<]>-.+.++++++++++.+[---->+<]>+++.-[--->++<]>-.++++++++++.+[---->+<]>+++.[-->+++++++<]>.++.-------------.[--->+<]>---..+++++.-[---->+<]>++.+[->+++<]>.++++++++++++..---.[-->+<]>--------.",
         "",
         "This is pretty cool.") // source: https://copy.sh/brainfuck/text.html

        ("++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.",
         "",
         "Hello World!\n")

        (",.", "A", "A") // echo input: read char and output it
        (",>,.<.", "AB", "BA") // reverse two chars
        (",>,[<+>-]<.", "12", "c") // add '1' (49) + '2' (50) = 99 = 'c'
        (",>,[<+>-]<++++++++++++++++++++++++++++++++++++++++++++++++.", "\u0005\u0005", ":") // add 5 + 5 = 10, then add 48 to get '10' → ':' (ASCII 58)
        (",>,[<+>-]<.", "A ", "a") // add 'A' (65) + space (32) = 97 = 'a'

        ("+++++++++[>++++++++++<-]>--.", "", "X") // output 'X' once
        ("+++++++++[>++++++++++<-]>--.....", "", "XXXXX") // output 'X' five times
        ("+++++++++[>++++++++<-]>.+.+.+.", "", "HIJK") // output multiple chars
    }

let runTests =
    testCases
    |> Seq.iteri (fun i (inputData, userInput, expected) ->
        let sw = System.Diagnostics.Stopwatch.StartNew()

        try
            let memorySize = 30_000
            let actual = Interpret memorySize inputData userInput
            sw.Stop()
            let elapsed = sw.Elapsed.TotalMilliseconds
            let ratio = float inputData.Length / float actual.Length
            let pass = expected = actual

            printfn
                $"""Test #%03d{i + 1}: {if pass then "✓" else "✗"} ({elapsed}ms, {actual.Length}b, ratio {ratio})"""

            if not pass then
                eprintfn
                    $"  %A{inputData} | %A{userInput} | expected: %A{expected} | actual: %A{actual}"
        with ex ->
            eprintfn $"Test #%03d{i + 1}: ✗ (Error: %A{ex.Message})")

[<EntryPoint>]
let main _argv =
    RunREPL()
    0
