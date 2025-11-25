open BFInterpreter.BF
open BFInterpreter.REPL

[<EntryPoint>]
let main args =
    let memorySize = 30_000

    match args with
    | [||] -> RunREPL()
    | [| inputData |] ->
        let output = Interpret memorySize inputData ""
        printfn $"{output}"
        0
    | [| inputData; userInput |] ->
        let output = Interpret memorySize inputData userInput
        printfn $"{output}"
        0
    | _ ->
        printfn "Usage: bfinterpreter [inputData] [userInput]"
        1
