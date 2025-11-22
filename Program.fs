/// Represents the possible `Brainf**k` operations.
type Operation =
    /// `'>'`: Increment the memory pointer (move to the next cell).
    | IncrementPointer
    /// `'<'`: Decrement the memory pointer (move to the previous cell).
    | DecrementPointer
    /// `'+'`: Increment the byte at the memory cell currently pointed to.
    | IncrementByte
    /// `'-'`: Decrement the byte at the memory cell currently pointed to.
    | DecrementByte
    /// `'.'`: Output the value at the memory cell currently pointed to as an ASCII character.
    | OutputByte
    /// `','`: Accept a single byte of user input and store it at the memory cell currently pointed to (ASCII value).
    | InputByte
    /// `'['`: Jump forward to the corresponding `]` if the current memory cell is zero.
    | JumpForward
    /// `']'`: Jump backward to the corresponding `[` if the current memory cell is non-zero.
    | JumpBackward

let charToOperation =
    function
    | '>' -> Operation.IncrementPointer
    | '<' -> Operation.DecrementPointer
    | '+' -> Operation.IncrementByte
    | '-' -> Operation.DecrementByte
    | '.' -> Operation.OutputByte
    | ',' -> Operation.InputByte
    | '[' -> Operation.JumpForward
    | ']' -> Operation.JumpBackward
    | _ -> failwith "Invalid BF operation"

let buildJumpMap (input: char[]) =
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
        (Map.empty, [])
    |> fst

let inline wrapPointer size ptr = (ptr % size + size) % size
let inline wrapByte n = (n % 256 + 256) % 256 |> byte

let interpret (inputData: string) (userInput: string) =
    let memorySize = 30_000
    let memory = memorySize |> Array.zeroCreate<byte> // BF memory (30_000 cells)
    let mutable pointer = 0 // memory pointer
    let mutable inputPointer = 0
    let mutable userInputPointer = 0
    let output = System.Text.StringBuilder()

    let jumpTable = inputData.ToCharArray() |> buildJumpMap

    while inputPointer < inputData.Length do
        match inputData.[inputPointer] |> charToOperation with
        | IncrementPointer -> pointer <- (pointer + 1) |> wrapPointer memorySize
        | DecrementPointer -> pointer <- (pointer - 1) |> wrapPointer memorySize
        | IncrementByte -> memory.[pointer] <- (int memory.[pointer] + 1) |> wrapByte
        | DecrementByte -> memory.[pointer] <- (int memory.[pointer] - 1) |> wrapByte
        | OutputByte -> output.Append(memory.[pointer] |> char) |> ignore
        | InputByte when userInputPointer < userInput.Length ->
            memory.[pointer] <- userInput.[userInputPointer] |> byte
            userInputPointer <- userInputPointer + 1
        | JumpForward when memory.[pointer] = 0uy -> inputPointer <- jumpTable.[inputPointer]
        | JumpBackward when memory.[pointer] <> 0uy -> inputPointer <- jumpTable.[inputPointer]
        | _ -> ()

        inputPointer <- inputPointer + 1

    output.ToString()

[<EntryPoint>]
let main _argv =
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
    |> Seq.iteri (fun i (inputData, userInput, expected) ->
        let actual = interpret inputData userInput
        let pass = if expected = actual then "✓" else "✗"

        if expected <> actual then
            printfn
                $"""Test %d{i + 1}: {pass}
                inputData:  %A{inputData}
                userInput:  %A{userInput}
                expected:   %A{expected}
                actual:     %A{actual}
                """
        else
            printfn $"""Test %d{i + 1}: {pass}""")

    0
