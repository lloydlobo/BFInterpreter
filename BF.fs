module BFInterpreter.BF

open System

/// Represents the possible Brainf**k operations.
/// Source: https://www.dcode.fr/brainfuck-language
type Operation =
    /// '>' : Move the memory pointer to the next cell.
    | IncrementPointer
    /// '<' : Move the memory pointer to the previous cell.
    | DecrementPointer
    /// '+' : Increment the value at the current memory cell.
    | IncrementByte
    /// '-' : Decrement the value at the current memory cell.
    | DecrementByte
    /// '.' : Output the current memory cell as an ASCII character.
    | OutputByte
    /// ',' : Read a single byte of input into the current memory cell.
    | InputByte
    /// '[' : Jump forward to the matching ']' if the current cell is zero.
    | JumpForward
    /// ']' : Jump backward to the matching '[' if the current cell is non-zero.
    | JumpBackward

/// Builds a jump table mapping '[' to ']' and vice versa.
let charToOperation =
    function
    | '>' -> Some Operation.IncrementPointer
    | '<' -> Some Operation.DecrementPointer
    | '+' -> Some Operation.IncrementByte
    | '-' -> Some Operation.DecrementByte
    | '.' -> Some Operation.OutputByte
    | ',' -> Some Operation.InputByte
    | '[' -> Some Operation.JumpForward
    | ']' -> Some Operation.JumpBackward
    | _ -> None

let inline buildJumpMap (input: char[]) =
    input
    |> Array.indexed
    |> Array.fold
        (fun (jumps, stack) (i, c) ->
            match c with
            | '[' -> (jumps, i :: stack)
            | ']' ->
                match stack with
                | openPos :: rest -> (jumps |> Map.add openPos i |> Map.add i openPos, rest)
                | [] -> failwith "Unmatched closing bracket `]`."
            | _ -> (jumps, stack))
        (Map.empty, [])
    |> fst

let inline wrapPointer size ptr = (ptr % size + size) % size // (cyclic memory)

let inline wrapByte n = (n % 256 + 256) % 256 |> byte // (0..255)

type State = { //~
    Memory: byte[]
    Pointer: int
    InputPointer: int
    UserInputPointer: int
    Output: char list
}

let Interpret memorySize (inputData: string) (userInput: string) =
    let operations =
        inputData
        |> Seq.mapi (fun i c ->
            match c |> charToOperation with
            | Some op -> op
            | None -> failwith $"Invalid character at {i}: {c}")
        |> Array.ofSeq

    let jumpTable = inputData.ToCharArray() |> buildJumpMap

    let rec loop (state: State) =
        let output () = Console.Write(char (state.Memory.[state.Pointer])) // TODO: Implement or remove

        let input () = state.Memory.[state.Pointer] <- int (Console.ReadKey(true).KeyChar) |> byte // TODO: if using interactive user input, then remove userInput param

        if state.InputPointer >= inputData.Length then
            state.Output |> List.rev |> Array.ofList |> String
        else
            let nextState =
                match operations.[state.InputPointer] with
                | IncrementPointer -> { state with Pointer = (state.Pointer + 1) |> wrapPointer memorySize }
                | DecrementPointer -> { state with Pointer = (state.Pointer - 1) |> wrapPointer memorySize }
                | IncrementByte ->
                    state.Memory.[state.Pointer] <- (int state.Memory.[state.Pointer] + 1) |> wrapByte
                    state
                | DecrementByte ->
                    state.Memory.[state.Pointer] <- (int state.Memory.[state.Pointer] - 1) |> wrapByte
                    state
                | OutputByte -> { state with Output = char state.Memory.[state.Pointer] :: state.Output }
                | InputByte ->
                    state.Memory.[state.Pointer] <-
                        match state.UserInputPointer < userInput.Length with
                        | true -> userInput.[state.UserInputPointer] |> byte
                        | _ -> 0uy

                    { state with UserInputPointer = state.UserInputPointer + 1 }
                | JumpForward when state.Memory.[state.Pointer] = 0uy -> { state with InputPointer = jumpTable.[state.InputPointer] }
                | JumpBackward when state.Memory.[state.Pointer] <> 0uy -> { state with InputPointer = jumpTable.[state.InputPointer] }
                | _ -> state

            loop { nextState with InputPointer = nextState.InputPointer + 1 } // tail-recursive call

    loop { Memory = memorySize |> Array.zeroCreate; Pointer = 0; InputPointer = 0; UserInputPointer = 0; Output = [] }
