// The `Brainf**k` works like a Turing Machine, with a read/write head (pointer)
// that manipulates a buffer. The 8 possible machine operations are:
//     ">" "<" "+" "-" "." "," "[" "]"
// Any other character is generally ignored by the interpreter.
// Source: https://www.dcode.fr/brainfuck-language
let legend =
    [ ('>', "increment the pointer position (+1)")
      ('<', "decrement the pointer position (-1)")
      ('+', "increment the byte in the memory cell where the pointer is located")
      ('-', "decrement the byte in the memory cell where the pointer is located")
      ('.', "send the value of the pointed byte as output (treated as an ASCII value)")
      (',', "insert an input byte (user input) in the memory cell where the pointer is located (ASCII value)")
      ('[', "if the pointed byte is 0 then jump to instruction after the corresponding ]")
      (']', "if the pointed byte is not 0 then jump to the instruction after the corresponding [") ]
    |> Map.ofList

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

let interpret (inputData: string) =
    let memorySize = 30_000
    let memory = Array.zeroCreate<byte> memorySize // BF memory (30_000 cells)
    let mutable pointer = 0 // memory pointer
    let mutable inputPointer = 0
    let output = System.Text.StringBuilder()

    let jumpTable = inputData.ToCharArray() |> buildJumpTable
    let newlineByte = 10uy
    let inputDataLength = inputData.Length
    let debug = false

    printfn $"[DEBUG] jumpTable=%A{jumpTable}"

    while inputPointer < inputDataLength do
        if debug then
            let location = $"[%03d{inputPointer}..%03d{inputDataLength}]"
            let key = inputData.[inputPointer]
            let value = legend |> Map.tryFind key |> Option.defaultValue "Unknown"
            eprintfn $"""[DEBUG] %s{location} %c{key}: %s{value}"""

        match inputData.[inputPointer] with
        | '>' -> pointer <- (pointer + 1) % memorySize
        | '<' -> pointer <- (pointer - 1 + memorySize) % memorySize
        | '+' -> memory.[pointer] <- (int memory.[pointer] + 1) % 256 |> byte
        | '-' -> memory.[pointer] <- (int memory.[pointer] - 1 + 256) % 256 |> byte
        | '.' ->
            match memory.[pointer] with
            | x when (x >= 32uy && x <= 126uy) || x = newlineByte -> output.Append(x |> char) |> ignore
            | x -> eprintfn $"[WARNING] Non-printable character at pointer %d{pointer}: %d{x}"
        | ',' -> // WARNING: this branch is untested
            if inputPointer < inputDataLength then
                memory.[pointer] <- inputData.[inputPointer] |> int |> byte
                inputPointer <- inputPointer + 1
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
             "Hello World!\n")

            ("-[--->+<]>-.[---->+++++<]>-.+.++++++++++.+[---->+<]>+++.-[--->++<]>-.++++++++++.+[---->+<]>+++.[-->+++++++<]>.++.-------------.[--->+<]>---..+++++.-[---->+<]>++.+[->+++<]>.++++++++++++..---.[-->+<]>--------.",
             "This is pretty cool.") // Source: https://copy.sh/brainfuck/text.html
        }

    testCases
    |> Seq.iter (fun (input, expected) ->
        let actual = input |> interpret
        printfn $"[DEBUG]\n    input:    %A{input}\n    actual:   %A{actual}\n    expected: %A{expected}\n")

    0

// [DEBUG] jumpTable=map [(8, 48); (14, 33); (33, 14); (43, 45); (45, 43); (48, 8)]
// [DEBUG]
//     input:    "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
//     actual:   "Hello World!
// "
//     expected: "Hello World!
// "
//
// [DEBUG] jumpTable=map
//   [(1, 8); (8, 1); (12, 24); (24, 12); (42, 50); (50, 42); (57, 65); (65, 57);
//    (81, 89); ...]
// [DEBUG]
//     input:    "-[--->+<]>-.[---->+++++<]>-.+.++++++++++.+[---->+<]>+++.-[--->++<]>-.++++++++++.+[---->+<]>+++.[-->+++++++<]>.++.-------------.[--->+<]>---..+++++.-[---->+<]>++.+[->+++<]>.++++++++++++..---.[-->+<]>--------."
//     actual:   "This is pretty cool."
//     expected: "This is pretty cool."
