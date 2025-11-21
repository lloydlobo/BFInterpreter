let inline wrapPointer size ptr = (ptr % size + size) % size

let inline wrapByte n = (n % 256 + 256) % 256 |> byte

let buildJumpTable (input: char[]) = // Build bidirectional map: '[' positions -> ']' positions and vice versa
    input
    |> Array.indexed
    |> Array.fold
        (fun (jumps, stack) (i, c) -> // jumps: map being built
            match c with // stack: list of unmatched `[` positions
            | '[' -> (jumps, i :: stack) // Don't know where to jump yet, so push position i to stack
            | ']' -> // Pop stack to get matching `[` position
                match stack with
                | openPos :: rest -> (jumps |> Map.add openPos i |> Map.add i openPos, rest)
                | [] -> (jumps, stack)
            | _ -> (jumps, stack))
        (Map.empty, []) // Initial state
    |> fst

let interpret (inputData: string) (userInput: string) =
    let memSize = 30_000
    let mem = memSize |> Array.zeroCreate<byte> // BF memory (30_000 cells)
    let mutable memPointer = 0 // memory pointer
    let mutable inputPointer = 0
    let mutable userInputPointer = 0
    let output = System.Text.StringBuilder()
    let inputDataLength = inputData.Length
    let userInputLength = userInput.Length
    let jumpTable = inputData.ToCharArray() |> buildJumpTable

    while inputPointer < inputDataLength do
        match inputData.[inputPointer] with
        | '>' -> memPointer <- (memPointer + 1) |> wrapPointer memSize
        | '<' -> memPointer <- (memPointer - 1) |> wrapPointer memSize
        | '+' -> mem.[memPointer] <- (int mem.[memPointer] + 1) |> wrapByte
        | '-' -> mem.[memPointer] <- (int mem.[memPointer] - 1) |> wrapByte
        | '.' -> output.Append(mem.[memPointer] |> char) |> ignore
        | ',' when userInputPointer < userInputLength ->
            mem.[memPointer] <- userInput.[userInputPointer] |> byte
            userInputPointer <- userInputPointer + 1
        | '[' when mem.[memPointer] = 0uy -> inputPointer <- jumpTable.[inputPointer]
        | ']' when mem.[memPointer] <> 0uy -> inputPointer <- jumpTable.[inputPointer]
        | _ -> () // ignore non-BF characters

        inputPointer <- inputPointer + 1

    output.ToString()

[<EntryPoint>]
let main _argv =
    let testCases =
        seq {
            ("++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.",
             "",
             "Hello World!\n")

            ("-[--->+<]>-.[---->+++++<]>-.+.++++++++++.+[---->+<]>+++.-[--->++<]>-.++++++++++.+[---->+<]>+++.[-->+++++++<]>.++.-------------.[--->+<]>---..+++++.-[---->+<]>++.+[->+++<]>.++++++++++++..---.[-->+<]>--------.",
             "",
             "This is pretty cool.") // source: https://copy.sh/brainfuck/text.html

            (",.", "A", "A") // echo input: read char and output it
            (",>,.<.", "AB", "BA") // reverse two chars
            (",>,[<+>-]<.", "12", "c") // add '1' (49) + '2' (50) = 99 = 'c'
            (",>,[<+>-]<++++++++++++++++++++++++++++++++++++++++++++++++.", "\u0005\u0005", ":") // add 5 + 5 = 10, then add 48 to get '10' → ':' (ASCII 58)
            (",>,[<+>-]<.", "A ", "a") // add 'A' (65) + space (32) = 97 = 'a'
            ("+++++++++[>++++++++++<-]>--.", "", "X") // FIXME: simple loop: output 'X' five times (just outputs once)
            ("+++++++++[>++++++++<-]>.+.+.+.", "", "HIJK") // output multiple chars
        }

    testCases
    |> Seq.iteri (fun i (inputData, userInput, expected) ->
        let actual = interpret inputData userInput
        let pass = if expected = actual then "✓" else "✗"

        printfn
            $"""[DEBUG]     test %d{i + 1}: {pass}
            inputData:  %A{inputData}
            userInput:  %A{userInput}
            expected:   %A{expected}
            actual:     %A{actual}
            """)

    0

// [DEBUG]     test 1: ✓
//             inputData:  "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
//             userInput:  ""
//             expected:   "Hello World!
// "
//             actual:     "Hello World!
// "
//
// [DEBUG]     test 2: ✓
//             inputData:  "-[--->+<]>-.[---->+++++<]>-.+.++++++++++.+[---->+<]>+++.-[--->++<]>-.++++++++++.+[---->+<]>+++.[-->+++++++<]>.++.-------------.[--->+<]>---..+++++.-[---->+<]>++.+[->+++<]>.++++++++++++..---.[-->+<]>--------."
//             userInput:  ""
//             expected:   "This is pretty cool."
//             actual:     "This is pretty cool."
//
// [DEBUG]     test 3: ✓
//             inputData:  ",."
//             userInput:  "A"
//             expected:   "A"
//             actual:     "A"
//
// [DEBUG]     test 4: ✓
//             inputData:  ",>,.<."
//             userInput:  "AB"
//             expected:   "BA"
//             actual:     "BA"
//
// [DEBUG]     test 5: ✓
//             inputData:  ",>,[<+>-]<."
//             userInput:  "12"
//             expected:   "c"
//             actual:     "c"
//
// [DEBUG]     test 6: ✓
//             inputData:  ",>,[<+>-]<++++++++++++++++++++++++++++++++++++++++++++++++."
//             userInput:  ""
//             expected:   ":"
//             actual:     ":"
//
// [DEBUG]     test 7: ✓
//             inputData:  ",>,[<+>-]<."
//             userInput:  "A "
//             expected:   "a"
//             actual:     "a"
//
// [DEBUG]     test 8: ✓
//             inputData:  "+++++++++[>++++++++++<-]>--."
//             userInput:  ""
//             expected:   "X"
//             actual:     "X"
//
// [DEBUG]     test 9: ✓
//             inputData:  "+++++++++[>++++++++<-]>.+.+.+."
//             userInput:  ""
//             expected:   "HIJK"
//             actual:     "HIJK"
