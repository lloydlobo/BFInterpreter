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

let interpret (inputData: string) =
    let memorySize = 30_000
    let memory = Array.zeroCreate<byte> memorySize // BF memory (30_000 cells)
    let mutable pointer = 0 // memory pointer
    let mutable inputPointer = 0
    let output = System.Text.StringBuilder()

    let newlineByte = 10uy
    let inputDataLength = inputData.Length
    let debug = false

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
        | ',' -> // NOTE: this branch is untested
            if inputPointer < inputDataLength then
                memory.[pointer] <- inputData.[inputPointer] |> int |> byte
                inputPointer <- inputPointer + 1
        | '[' ->
            if memory.[pointer] = 0uy then
                let mutable openBrackets = 1

                while openBrackets > 0 && inputPointer < inputDataLength do
                    inputPointer <- inputPointer + 1

                    match inputData.[inputPointer] with
                    | '[' -> openBrackets <- openBrackets + 1
                    | ']' -> openBrackets <- openBrackets - 1
                    | _ -> ()
        | ']' ->
            if memory.[pointer] <> 0uy then
                let mutable closeBrackets = 1

                while closeBrackets > 0 && inputPointer > 0 do
                    inputPointer <- inputPointer - 1

                    match inputData.[inputPointer] with
                    | ']' -> closeBrackets <- closeBrackets + 1
                    | '[' -> closeBrackets <- closeBrackets - 1
                    | _ -> ()
        | _ -> () // ignore non-BF characters

        inputPointer <- inputPointer + 1

    output.ToString()

[<EntryPoint>]
let main _argv =
    let testCases =
        seq {
            ("++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.",
             "Hello, World!\n")

            ("-[--->+<]>-.[---->+++++<]>-.+.++++++++++.+[---->+<]>+++.-[--->++<]>-.++++++++++.+[---->+<]>+++.[-->+++++++<]>.++.-------------.[--->+<]>---..+++++.-[---->+<]>++.+[->+++<]>.++++++++++++..---.[-->+<]>--------.",
             "This is pretty cool.") // Source: https://copy.sh/brainfuck/text.html
        }

    testCases
    |> Seq.iter (fun (input, expected) ->
        let actual = input |> interpret
        printfn $"[DEBUG]\n    input=%A{input}\n    expected=%A{expected}\n    actual=%A{actual}")

    0

// [DEBUG]
//     input="++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
//     expected="Hello, World!
// "
//     actual="Hello World!
// "
// [DEBUG]
//     input="-[--->+<]>-.[---->+++++<]>-.+.++++++++++.+[---->+<]>+++.-[--->++<]>-.++++++++++.+[---->+<]>+++.[-->+++++++<]>.++.-------------.[--->+<]>---..+++++.-[---->+<]>++.+[->+++<]>.++++++++++++..---.[-->+<]>--------."
//     expected="This is pretty cool."
//     actual="This is pretty cool."
