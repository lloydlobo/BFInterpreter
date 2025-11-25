module BFInterpreter.REPL

open System
open BFInterpreter.BF

let helpMessage =
    """
  Interactive directives:

    clear                                         // Clear screen
    help                                          // Display help
    quit                                          // Exit

  Operations:

    >                                             // Move right
    <                                             // Move left
    +                                             // Increment
    -                                             // Decrement
    .                                             // Output character
    ,                                             // Input character
    [                                             // Jump forward if zero
    ]                                             // Jump backward if not zero

  Examples:

    +++++++++[>++++++++++<-]>--.                  // X
    +++++++++[>++++++++++<-]>--.....              // XXXXX
    +++++++++[>++++++++<-]>.+.+.+.                // HJKL

    >+++++++[<++++++++>-]<++.                     // :
    >++++++++[<++++++++>-]<+.                     // A
    >++++++++++[<++++++++++>-]<+++++.             // i
    """

// Read-eval-print loop (See: https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop)
let RunREPL () =
    Console.WriteLine()
    Console.WriteLine("""Brainfuck REPL""")
    Console.WriteLine()
    Console.WriteLine("For help type 'help'")
    Console.WriteLine()

    let handleProgram input =
        let memorySize = 30_000
        let userInput = "" // You can modify this to read user input for ',' (Brainfuck input)
        let output = Interpret memorySize input userInput
        Console.WriteLine($"{output}\n")

    let mutable running = true

    while running do
        Console.Write("> ")
        let input = Console.ReadLine()

        if input |> isNull then
            eprintfn "No input available. Exiting" // NOTE: gets triggered on Ctrl-d
            running <- false
        else
            match input.ToLower() with
            | "quit" ->
                printfn "Exiting REPL."
                running <- false
            | "clear" -> Console.Clear()
            | "help" -> printfn $"{helpMessage}"
            | _ -> input |> handleProgram

    0
