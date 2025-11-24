open BFInterpreter.BF
open BFInterpreter.REPL
open Expecto.CSharp

[<EntryPoint>]
let main args =
    match args with
    | [||] -> RunREPL() // interacitve mode
    | [| bfCode |] ->
        let memorySize = 30_000
        let userInput = ""
        let output = Interpret memorySize bfCode userInput
        printfn $"{output}"
        0
    | _ ->
        printfn "Usage: bfinterpreter [bf-code]"
        1
