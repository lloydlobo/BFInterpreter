module BFInterpreter.BF

type State = { //~
    Memory: byte[]
    Pointer: int
    InputPointer: int
    UserInputPointer: int
    Output: char list
}

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
                | [] -> failwith "Unmatched closing bracket `]`."
            | _ -> (jumps, stack))
        (Map.empty, [])
    |> fst

let inline wrapPointer size ptr = (ptr % size + size) % size // (cyclic memory)

let inline wrapByte n = (n % 256 + 256) % 256 |> byte // (0..255)

let Interpret memorySize (inputData: string) (userInput: string) =
    let operations = inputData |> Seq.choose charToOperation |> Array.ofSeq

    if operations.Length <> inputData.Length then
        failwith "Invalid character in BF source"

    let jumpTable = inputData.ToCharArray() |> buildJumpMap

    let mutateCell ptr delta (memory: byte[]) = // “mutate cell at ptr by delta in memory”
        memory.[ptr] <- (int memory.[ptr] + delta) |> wrapByte

    let rec loop (state: State) =
        if state.InputPointer >= inputData.Length then
            state.Output |> List.rev |> Array.ofList |> System.String
        else
            let nextState =
                match operations.[state.InputPointer] with
                | IncrementPointer -> { state with Pointer = (state.Pointer + 1) |> wrapPointer memorySize }
                | DecrementPointer -> { state with Pointer = (state.Pointer - 1) |> wrapPointer memorySize }
                | IncrementByte ->
                    state.Memory |> mutateCell state.Pointer 1 |> ignore
                    state
                | DecrementByte ->
                    state.Memory |> mutateCell state.Pointer -1 |> ignore
                    state
                | OutputByte -> { state with Output = char state.Memory.[state.Pointer] :: state.Output }
                | InputByte when state.UserInputPointer < userInput.Length ->
                    state.Memory.[state.Pointer] <- userInput.[state.UserInputPointer] |> byte
                    { state with UserInputPointer = state.UserInputPointer + 1 }
                | JumpForward when state.Memory.[state.Pointer] = 0uy -> { state with InputPointer = jumpTable.[state.InputPointer] }
                | JumpBackward when state.Memory.[state.Pointer] <> 0uy -> { state with InputPointer = jumpTable.[state.InputPointer] }
                | _ -> state

            loop { nextState with InputPointer = nextState.InputPointer + 1 } // tail-recursive call

    loop {
        Memory = memorySize |> Array.zeroCreate //~
        Pointer = 0
        InputPointer = 0
        UserInputPointer = 0
        Output = []
    }
