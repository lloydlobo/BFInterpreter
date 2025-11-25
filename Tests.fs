module BFInterpreter.Tests

open System.Diagnostics // provides: Stopwatch
open Expecto // provides: Expect, test, testList
open FsCheck // provides: testProperty
open BFInterpreter.BF // provides: Interpret, wrapByte, wrapPointer

[<Tests>]
let brainfuckTests =
    testList "Brainfuck Interpreter Tests" [
        testList
            "Brainfuck Test Cases"
            (Seq.append
                (seq [
                    (",.", "A", "A", "echo input: read char and output it")
                    (",>,.<.", "AB", "BA", "reverse two chars")
                    (",>,[<+>-]<.", "12", "c", "add '1' (49) + '2' (50) = 99 = 'c'")
                    (",>,[<+>-]<++++++++++++++++++++++++++++++++++++++++++++++++.", "\u0005\u0005", ":", "5+5=10, add 48 = '10' → ':' (ASCII 58)")
                    (",>,[<+>-]<.", "A ", "a", "add 'A' (65) + space (32) = 97 = 'a'")
                    ("+++++++++[>++++++++++<-]>--.", "", "X", "output 'X' once")
                    ("+++++++++[>++++++++++<-]>--.....", "", "XXXXX", "output 'X' five times")
                    ("+++++++++[>++++++++<-]>.+.+.+.", "", "HIJK", "output multiple chars")
                ])
                (seq [
                    ("++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.", "", "Hello World!\n", "")
                    ("-[--->+<]>-.[---->+++++<]>-.+.++++++++++.+[---->+<]>+++.-[--->++<]>-.++++++++++.+[---->+<]>+++.[-->+++++++<]>.++.-------------.[--->+<]>---..+++++.-[---->+<]>++.+[->+++<]>.++++++++++++..---.[-->+<]>--------.",
                     "",
                     "This is pretty cool.",
                     "source: https://copy.sh/brainfuck/text.html")
                ])
             |> Seq.mapi (fun i (inputData, userInput, expected, msg) ->
                 testCase $"Test #%03d{i + 1}"
                 <| fun _ ->
                     let sw = Stopwatch.StartNew()
                     let memorySize = 30_000

                     try
                         let actual = Interpret memorySize inputData userInput
                         sw.Stop()
                         let elapsed = sw.Elapsed.TotalMilliseconds
                         let ratio = float inputData.Length / float actual.Length
                         let pass = expected = actual
                         Expect.equal actual expected $"Test #%03d{i + 1}: Expected output did not match actual output. {msg}"
                         Expect.equal actual.Length expected.Length $"Test #%03d{i + 1}: Expected and actual output lengths are different."
                         printfn $"""Test #%03d{i + 1}: {if pass then "✓" else "✗"} ({elapsed}ms, {actual.Length}b, ratio {ratio})"""
                     with ex ->
                         Tests.failtest $"Test #%03d{i + 1}: Error: {ex.Message}")
             |> Seq.toList)
    ]

[<Tests>]
let operationTests = //~
    testList "operation utils" [
        testCase "Valid char-to-operation mappings"
        <| fun _ ->
            seq {
                ('>', Some Operation.IncrementPointer)
                ('<', Some Operation.DecrementPointer)
                ('+', Some Operation.IncrementByte)
                ('-', Some Operation.DecrementByte)
                ('.', Some Operation.OutputByte)
                (',', Some Operation.InputByte)
                ('[', Some Operation.JumpForward)
                (']', Some Operation.JumpBackward)
            }
            |> Seq.iter (fun (c, op) -> Expect.equal (charToOperation c) op $"Expected {op} for '{c}'")

        testCase "Invalid char mappings"
        <| fun _ -> seq [ 'a'; '1'; '!'; ' ' ] |> Seq.iter (fun c -> Expect.isNone (charToOperation c) $"Expected None for '{c}'")

        testProperty ""
        <| fun (c: char) ->
            let valid =
                seq [
                    ('>', Some Operation.IncrementPointer)
                    ('<', Some Operation.DecrementPointer)
                    ('+', Some Operation.IncrementByte)
                    ('-', Some Operation.DecrementByte)
                    ('.', Some Operation.OutputByte)
                    (',', Some Operation.InputByte)
                    ('[', Some Operation.JumpForward)
                    (']', Some Operation.JumpBackward)
                ]

            match c |> charToOperation with
            | Some actual ->
                match (valid |> Seq.tryFind (fun (ch, _) -> ch = c)) with
                | Some(_, Some expectedOp) -> Expect.equal actual expectedOp "Operation should match"
                | _ -> failwith "Expected operation should be Some, but was None"
            | None -> Expect.isNone (valid |> Seq.tryFind (fun (ch, _) -> ch = c)) "Invalid character should map to None"
    ]

[<Tests>]
let buildJumpMapUnitTests =
    testList "buildJumpMap Unit Tests" [
        testCase "Test simple matching brackets"
        <| fun _ ->
            let program = [| '['; '['; '>'; '>'; ']'; '<'; ']' |]
            let expected = [| 6; 4; -1; -1; 1; -1; 0 |]

            let operations =
                match program |> tryParseProgram with
                | Error parseError -> failwith $"{parseError}"
                | Ok ops -> ops

            let result = operations |> buildJumpMapFromOperations
            Expect.equal result expected "The jump map should correctly match the brackets."

        testCase "Test empty input"
        <| fun _ ->
            let program = [| '.'; '>' |] // No brackets
            let expected = [| -1; -1 |]

            let operations =
                match program |> tryParseProgram with
                | Error parseError -> failwith $"{parseError}"
                | Ok ops -> ops

            let result = operations |> buildJumpMapFromOperations
            Expect.equal result expected "The jump map should be empty for input without brackets."
    ]

[<Tests>]
let wrapUnitTests =
    testList "wrap*** Unit Tests" [
        testProperty "wrapByte returns values in the range 0..255: `(n%256+256)%256|>byte`"
        <| fun n ->
            let actual = n |> wrapByte
            Expect.isTrue (actual >= 0uy && actual <= 255uy) "wrapByte should wrap in the range 0..255"

        testProperty "wrapPointer behaves correctly (cyclic memory): `(ptr%size+size)%size`"
        <| fun size ptr ->
            if size > 0 && ptr > 0 then
                let expectedForward = (ptr + 1) % size
                let actualForward = ((ptr + 1) |> wrapPointer size)
                Expect.equal actualForward expectedForward "wrapPointer should wrap forward"

                let expectedBackward = (ptr - 1 + size) % size
                let actualBackward = ((ptr - 1) |> wrapPointer size)
                Expect.equal actualBackward expectedBackward "wrapPointer should wrap backward"
    ]

[<Tests>]
let miscTests =
    testList "Brainfuck Misc. Tests" [
        testCase "Empty program"
        <| fun _ ->
            let result = Interpret 30000 "" ""
            Expect.equal result "" "Empty program should result in empty output."

        testCase "Empty loop"
        <| fun _ ->
            let result = Interpret 30000 "[->+<]" ""
            Expect.equal result "" "Empty loops should result in empty output."

        ptest "Large input" { // `p` pending test
            let input = String.replicate 1000 "," // A thousand comma inputs
            let expected = String.replicate 1000 "A" // Should output 'A' for each input
            let result = Interpret 30000 ",.,.,.,.,." input
            Expect.equal result expected "Large input should produce corresponding output."
        }

        testCase "Memory boundary test"
        <| fun _ ->
            let program = ">" // Move pointer to 1, but test with a small memory size
            let memorySize = 10
            let result = Interpret memorySize program ""
            Expect.equal result "" "Pointer should wrap within memory bounds."

        ptestCase "Memory wrap-around test"
        <| fun _ ->
            let program = ">++++[<+>-]<<<." // Program that manipulates memory and checks wrap
            let expected = ":"
            let result = Interpret 30000 program ""
            Expect.equal result expected "Memory should wrap around correctly."

        testCase "Pointer wrap-around test"
        <| fun _ ->
            let program = ">" // Moving pointer beyond the bounds
            let memorySize = 10
            let result = Interpret memorySize program ""
            Expect.equal result "" "Pointer should wrap around correctly within the memory size."

        ptestProperty "Valid Brainfuck program syntax"
        <| fun (program: string) ->
            let isValidBrainfuck =
                program
                |> Seq.fold
                    (fun (openBrackets, result) c ->
                        match c with
                        | '[' -> (openBrackets + 1, result)
                        | ']' ->
                            if openBrackets > 0 then
                                (openBrackets - 1, result)
                            else
                                (openBrackets, false)
                        | _ -> (openBrackets, result))
                    (0, true)
                |> snd

            Expect.isTrue isValidBrainfuck "Program should have balanced brackets and be valid syntax."

        testProperty "wrapPointer with different sizes"
        <| fun size ptr ->
            if size > 0 then
                let wrappedPtr = wrapPointer size ptr
                Expect.isTrue (wrappedPtr >= 0 && wrappedPtr < size) "Pointer should wrap within the bounds of memory."

        ptestCase "Stress test large program"
        <| fun _ ->
            let program = String.replicate 10000 "+" // A program with many operations
            let result = Interpret 30000 program ""
            Expect.isTrue (result.Length > 0) "Stress test should produce output."

        ptestCase "Large input/output program"
        <| fun _ ->
            let input = String.replicate 10000 "," // Long input stream
            let program = String.replicate 10000 "." // Output the input back
            let result = Interpret 30000 program input
            Expect.equal result (String.replicate 10000 "A") "Program should echo large input correctly."

        testCase "Timeout for long-running program"
        <| fun _ ->
            let program = String.replicate 100000 ">" // Extremely long pointer-moving program
            let sw = Stopwatch.StartNew()

            try
                let _ = Interpret 30000 program ""
                sw.Stop()
                Expect.isTrue (sw.ElapsedMilliseconds < 500) "Program should run within a reasonable time."
            with ex ->
                failwithf $"Test exceeded time limit: %s{ex.Message}"
    ]
