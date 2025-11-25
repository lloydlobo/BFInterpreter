// ReSharper disable FSharpRedundantDotInIndexer

module BFInterpreter.BF

open System
open System.Diagnostics // provides: Debug.Assert

type ParseError =
    | InvalidCharacters of (char * int) list
    | UnbalancedBrackets of int // net bracket count
    | MismatchedBrackets of int * int

type InterpreterError =
    | ParseError of ParseError
    | InvalidMemorySize of int
    | RuntimeError of string

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

module Constants =
    let MaxMemorySize = 1_048_576 // 1MB max
    let DefaultMemorySize = 30_000

// TODO: Performance note: Jump table uses Map instead of array for O(1) access
let inline buildJumpMapFromOperations (operations: Operation[]) =
    operations
    |> Array.indexed
    |> Array.fold
        (fun (jumps, stack) (i, op) ->
            match op with
            | Operation.JumpForward -> (jumps, i :: stack)
            | Operation.JumpBackward ->
                match stack with
                | openPos :: rest -> (jumps |> Map.add openPos i |> Map.add i openPos, rest)
                | [] -> failwith "Unmatched JumpBackward operation." // NOTE: avoid this maybe so early on? (either log it or return all errors)
            | _ -> (jumps, stack))
        (Map.empty, [])
    |> fst

let inline wrapPointer size ptr = // (cyclic memory)
    (ptr % size + size) % size

let inline wrapByte n = // (0..255)
    (n &&& 0XFF) |> byte // similar to (n % 256 + 256) % 256 |> byte

let constrainMemorySize =
    function
    | x when x <= 0 || x > Constants.MaxMemorySize -> Error(InvalidMemorySize x)
    | x -> Ok x

type State = { //~
    Memory: byte[] // Memory: System.Span<byte>
    Pointer: int
    InputPointer: int
    UserInputPointer: int
    OutputBuilder: System.Text.StringBuilder
}

let tryParseProgram inputData : Result<Operation array, ParseError> =
    let ops = ResizeArray()
    let invalidChars = ResizeArray()

    inputData
    |> Seq.iteri (fun i c ->
        match c |> charToOperation with
        | Some op -> ops.Add(op)
        | None -> invalidChars.Add((c, i)))

    if invalidChars.Count > 0 then
        Error(InvalidCharacters(invalidChars |> Seq.toList))
    else
        let ops = ops.ToArray()

        let bracketCount =
            ops
            |> Array.sumBy (function
                | JumpForward -> 1
                | JumpBackward -> -1
                | _ -> 0)

        match bracketCount = 0 with
        | true -> Ok ops
        | false -> Error(UnbalancedBrackets bracketCount)

let TryInterpret (memorySize: int) (inputData: string) (userInput: string) : Result<String, InterpreterError> =
    match memorySize |> constrainMemorySize with
    | Error x -> Error x
    | Ok memorySize' ->
        match inputData |> tryParseProgram with
        | Error parseError -> Error(ParseError parseError)
        | Ok operations ->
            try
                let jumpTable = operations |> buildJumpMapFromOperations
                assert (jumpTable.Count % 2 = 0) // paired brackets

                let rec loop (state: State) =
                    if state.InputPointer >= operations.Length then
                        assert (state.InputPointer = operations.Length)
                        Ok(state.OutputBuilder.ToString())
                    else
                        let nextState =
                            match operations.[state.InputPointer] with
                            | IncrementPointer -> { state with Pointer = (state.Pointer + 1) |> wrapPointer memorySize' }
                            | DecrementPointer -> { state with Pointer = (state.Pointer - 1) |> wrapPointer memorySize' }
                            | IncrementByte ->
                                state.Memory.[state.Pointer] <- (int state.Memory.[state.Pointer] + 1) |> wrapByte
                                state
                            | DecrementByte ->
                                state.Memory.[state.Pointer] <- (int state.Memory.[state.Pointer] - 1) |> wrapByte
                                state
                            | OutputByte ->
                                state.OutputBuilder.Append(char state.Memory.[state.Pointer]) |> ignore
                                state
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

                loop {
                    Memory = memorySize' |> Array.zeroCreate
                    Pointer = 0
                    InputPointer = 0
                    UserInputPointer = 0
                    OutputBuilder = System.Text.StringBuilder()
                }
            with ex ->
                Error(RuntimeError ex.Message)

let Interpret memorySize inputData userInput =
    match TryInterpret memorySize inputData userInput with
    | Ok result -> result
    | Error error ->
        let errorMessage =
            match error with
            | ParseError err ->
                match err with
                | InvalidCharacters chars ->
                    chars |> List.map (fun (c, i) -> $"Invalid character '{c}' at position {i}. Only >, <, +, -, ., ,, [, ] are allowed.") |> String.concat "; "
                | UnbalancedBrackets count -> $"Unbalanced brackets in program (net count: {count})"
            | InvalidMemorySize size -> $"Invalid memory size: {size}. Must be between 1 and {Constants.MaxMemorySize}"
            | RuntimeError msg -> $"Runtime error: {msg}"

        failwith errorMessage

// 6. Performance Optimizations
// - Pre-allocate output buffer with estimated size
// - Consider jump table as array instead of Map for O(1) access
