// The `Brainf**k` works like a Turing Machine, with a read/write head (pointer)
// that manipulates a buffer. The 8 possible machine operations are:
//     ">" "<" "+" "-" "." "," "[" "]"
// Any other character is generally ignored by the interpreter.
// Source: https://www.dcode.fr/brainfuck-language
let operationMap =
    [ ('>', "increment the pointer position (+1)")
      ('<', "decrement the pointer position (-1)")
      ('+', "increment the byte in the memory cell where the pointer is located")
      ('-', "decrement the byte in the memory cell where the pointer is located")
      ('.', "send the value of the pointed byte as output (treated as an ASCII value)")
      (',', "insert an input byte (user input) in the memory cell where the pointer is located (ASCII value)")
      ('[', "if the pointed byte is 0 then jump to instruction after the corresponding ]")
      (']', "if the pointed byte is not 0 then jump to the instruction after the corresponding [") ]
    |> Map.ofList

let debugFrame (inputData: string) inputPointer =
    let location = $"[%03d{inputPointer}..%03d{inputData.Length}]"
    let key = inputData.[inputPointer]
    let value = operationMap |> Map.tryFind key |> Option.defaultValue "Unknown"
    eprintfn $"""[DEBUG] %s{location} %c{key}: %s{value}"""

// Build bidirectional map: '[' positions -> ']' positions and vice versa
// • jumps: map being built
// • stack: list of unmatched `[` positions
// • '[' don't know where to jump yet, so push position i to stack
// • ']' pop stack to get matching `[` position
// • '_' non-bracket: ignore, pass state through unchanged.
let buildJumpTable (input: char[]) =
    input
    |> Array.indexed
    |> Array.fold
        (fun (jumps, stack) (i, c) ->
            match c with
            | '[' -> (jumps, i :: stack)
            | ']' ->
                match stack with
                | openPos :: rest -> (jumps |> Map.add openPos i |> Map.add i openPos, rest)
                | [] -> (jumps, stack)
            | _ -> (jumps, stack))
        (Map.empty, []) // initial state
    |> fst

let interpret (inputData: string) (userInput: string) =
    let memorySize = 30_000
    let memory = Array.zeroCreate<byte> memorySize // BF memory (30_000 cells)
    let mutable pointer = 0 // memory pointer
    let mutable inputPointer = 0
    let mutable userInputPointer = 0
    let output = System.Text.StringBuilder()

    let jumpTable = inputData.ToCharArray() |> buildJumpTable
    let inputDataLength = inputData.Length
    let userInputLength = userInput.Length
    let debug = false

    if debug then
        printfn $"[DEBUG] jumpTable=%A{jumpTable}"

    while inputPointer < inputDataLength do
        if debug then
            inputPointer |> debugFrame inputData

        match inputData.[inputPointer] with
        | '>' -> pointer <- (pointer + 1) % memorySize
        | '<' -> pointer <- (pointer - 1 + memorySize) % memorySize
        | '+' -> memory.[pointer] <- (int memory.[pointer] + 1) % 256 |> byte
        | '-' -> memory.[pointer] <- (int memory.[pointer] - 1 + 256) % 256 |> byte
        | '.' -> output.Append(memory.[pointer] |> char) |> ignore
        | ',' -> // WARNING: this branch is untested
            if userInputPointer < userInputLength then
                memory.[pointer] <- userInput.[userInputPointer] |> byte
                userInputPointer <- userInputPointer + 1
        | '[' when memory.[pointer] = 0uy -> inputPointer <- jumpTable.[inputPointer]
        | ']' when memory.[pointer] <> 0uy -> inputPointer <- jumpTable.[inputPointer]
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
