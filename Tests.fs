module BFInterpreter.Tests

open System.Diagnostics // provides: Stopwatch
open Expecto // provides: Expect, test, testList
open FsCheck // provides: testProperty
open BFInterpreter.BF // provides: Interpret, wrapByte, wrapPointer


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
let buildJumpMapTests =
    testList "buildJumpMap Tests" [

        testCase "Test simple matching brackets"
        <| fun _ ->
            let program = [| '['; '['; '>'; '>'; ']'; '<'; ']' |]
            let expected = Map.ofList [ (0, 6); (6, 0); (1, 4); (4, 1) ]
            let result = buildJumpMap program
            Expect.equal result expected "The jump map should correctly match the brackets."

        testCase "Test empty input"
        <| fun _ ->
            let program = [| '.'; '>' |] // No brackets
            let expected = Map.empty
            let result = buildJumpMap program
            Expect.equal result expected "The jump map should be empty for input without brackets."
    ]

[<Tests>]
let wrapTests =
    testList "wrap utils" [
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

let testCases =
    Seq.append
        (seq [
            (",.", "A", "A", "echo input: read char and output it")
            (",>,.<.", "AB", "BA", "reverse two chars")
            (",>,[<+>-]<.", "12", "c", "add '1' (49) + '2' (50) = 99 = 'c'")
            (",>,[<+>-]<++++++++++++++++++++++++++++++++++++++++++++++++.", "\u0005\u0005", ":", "add 5 + 5 = 10, then add 48 to get '10' → ':' (ASCII 58)")
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

[<Tests>]
let brainfuckTests =
    testList "Brainfuck Interpreter Tests" [
        testList
            "Brainfuck Test Cases"
            (testCases
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
