module BFInterpreter.REPL

open System
open BFInterpreter.BF

let helpMessage =
    """
  Interactive directives:

    clear                                         // Clear screen
    help                                          // Display help
    quit                                          // Exit

  Brainfuck operations:

    '>' : Move the memory pointer to the next cell.
    '<' : Move the memory pointer to the previous cell.
    '+' : Increment the value at the current memory cell.
    '-' : Decrement the value at the current memory cell.
    '.' : Output the current memory cell as an ASCII character.
    ',' : Read a single byte of input into the current memory cell.
    '[' : Jump forward to the matching ']' if the current cell is zero.
    ']' : Jump backward to the matching '[' if the current cell is non-zero.
    """

// Read-eval-print loop (See: https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop)
let rec RunREPL () =
    Console.WriteLine()
    Console.WriteLine("""Brainfuck REPL""")
    Console.WriteLine()
    Console.WriteLine("For help type 'help'")
    Console.WriteLine()

    let handleProgram input =
        let memorySize = 30_000
        let userInput = "" // You can modify this to read user input for ',' (Brainfuck input)
        let output = Interpret memorySize input userInput
        printfn $"{output}\n"

    let mutable running = true

    while running do
        Console.Write("> ")
        let input = Console.ReadLine()

        match input.ToLower() with
        | "quit" ->
            printfn "Exiting REPL."
            running <- false
        | "clear" -> Console.Clear()
        | "help" -> printfn $"{helpMessage}"
        | _ -> input |> handleProgram
