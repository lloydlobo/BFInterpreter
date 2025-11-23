module BFInterpreter.Tests

open System.Diagnostics // provides: Stopwatch
open Expecto // provides: Expect, test, testList
open FsCheck // provides: testProperty
open BFInterpreter.BF // provides: Interpret, wrapByte, wrapPointer

[<Tests>]
let tests =
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
    let seq1 =
        seq {
            (",.", "A", "A") // echo input: read char and output it
            (",>,.<.", "AB", "BA") // reverse two chars
            (",>,[<+>-]<.", "12", "c") // add '1' (49) + '2' (50) = 99 = 'c'
            (",>,[<+>-]<++++++++++++++++++++++++++++++++++++++++++++++++.", "\u0005\u0005", ":") // add 5 + 5 = 10, then add 48 to get '10' → ':' (ASCII 58)
            (",>,[<+>-]<.", "A ", "a") // add 'A' (65) + space (32) = 97 = 'a'

            ("+++++++++[>++++++++++<-]>--.", "", "X") // output 'X' once
            ("+++++++++[>++++++++++<-]>--.....", "", "XXXXX") // output 'X' five times
            ("+++++++++[>++++++++<-]>.+.+.+.", "", "HIJK") // output multiple chars
        }

    let seq2 =
        seq {
            ("-[--->+<]>-.[---->+++++<]>-.+.++++++++++.+[---->+<]>+++.-[--->++<]>-.++++++++++.+[---->+<]>+++.[-->+++++++<]>.++.-------------.[--->+<]>---..+++++.-[---->+<]>++.+[->+++<]>.++++++++++++..---.[-->+<]>--------.",
             "",
             "This is pretty cool.") // source: https://copy.sh/brainfuck/text.html

            ("++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.", "", "Hello World!\n")
        }

    Seq.append seq1 seq1


[<Tests>]
let brainfuckTests =
    testList "Brainfuck Interpreter Tests" [
        testList
            "Brainfuck Test Cases"
            (testCases
             |> Seq.mapi (fun i (inputData, userInput, expected) ->
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
                         Expect.equal actual expected $"Test #%03d{i + 1}: Expected output did not match actual output."
                         Expect.equal actual.Length expected.Length $"Test #%03d{i + 1}: Expected and actual output lengths are different."
                         printfn $"""Test #%03d{i + 1}: {if pass then "✓" else "✗"} ({elapsed}ms, {actual.Length}b, ratio {ratio})"""
                     with ex ->
                         Tests.failtest $"Test #%03d{i + 1}: Error: {ex.Message}")
             |> Seq.toList)
    ]
