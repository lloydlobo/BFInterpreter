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
                | [] -> failwith "Unmatched closing bracket `]`." // (jumps, stack)
            | _ -> (jumps, stack))
        (Map.empty, [])
    |> fst

let inline wrapPointer size ptr = (ptr % size + size) % size

let inline wrapByte n = (n % 256 + 256) % 256 |> byte

type State =
    { Memory: byte[] //~
      Pointer: int
      InputPointer: int
      UserInputPointer: int
      Output: char list }

let interpret memorySize (inputData: string) (userInput: string) =
    inputData
    |> Seq.iter (fun c ->
        if "><+-.,[]".IndexOf(c) = -1 then
            failwith "Invalid character in BF source")

    let jumpTable = inputData.ToCharArray() |> buildJumpMap

    let rec loop (state: State) =
        if state.InputPointer >= inputData.Length then
            state.Output |> List.rev |> Array.ofList |> System.String
        else
            let nextState =
                match (inputData.[state.InputPointer] |> charToOperation) with
                | IncrementPointer -> { state with Pointer = (state.Pointer + 1) |> wrapPointer memorySize }
                | DecrementPointer -> { state with Pointer = (state.Pointer - 1) |> wrapPointer memorySize }
                | IncrementByte ->
                    state.Memory.[state.Pointer] <- (int state.Memory.[state.Pointer] + 1) |> wrapByte
                    state
                | DecrementByte ->
                    state.Memory.[state.Pointer] <- (int state.Memory.[state.Pointer] - 1) |> wrapByte
                    state
                | OutputByte -> { state with Output = char state.Memory.[state.Pointer] :: state.Output }
                | InputByte when state.UserInputPointer < userInput.Length ->
                    state.Memory.[state.Pointer] <- userInput.[state.UserInputPointer] |> byte
                    { state with UserInputPointer = state.UserInputPointer + 1 }
                | JumpForward when state.Memory.[state.Pointer] = 0uy ->
                    { state with InputPointer = jumpTable.[state.InputPointer] }
                | JumpBackward when state.Memory.[state.Pointer] <> 0uy ->
                    { state with InputPointer = jumpTable.[state.InputPointer] }
                | _ -> state

            loop { nextState with InputPointer = nextState.InputPointer + 1 } // tail-recursive call

    loop { Memory = memorySize |> Array.zeroCreate; Pointer = 0; InputPointer = 0; UserInputPointer = 0; Output = [] }

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

[<EntryPoint>]
let main _argv =
    testCases
    |> Seq.iteri (fun i (inputData, userInput, expected) ->
        let sw = System.Diagnostics.Stopwatch.StartNew()

        try
            let memorySize = 30_000
            let actual = interpret memorySize inputData userInput
            sw.Stop()

            let elapsed = sw.Elapsed.TotalMilliseconds
            let ratio = float inputData.Length / float actual.Length
            let pass = expected = actual

            printfn
                $"""Test #%03d{i + 1}: {if pass then "✓" else "✗"} ({elapsed}ms, {actual.Length}b, ratio {ratio})"""

            if not pass then
                eprintfn $"  %A{inputData} | %A{userInput} | expected: %A{expected} | actual: %A{actual}"
        with ex ->
            eprintfn $"Test #%03d{i + 1}: ✗ (Error: %A{ex.Message})" //~
    )

    0
