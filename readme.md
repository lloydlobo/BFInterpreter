# BFInterpreter

A Brainfuck interpreter written in F# with REPL and comprehensive testing.

## Usage

```bash
# Interactive REPL mode
dotnet run
```

```bash
# Execute Brainfuck code directly
dotnet run "+++++++++[>++++++++<-]>.+.+.+." # HIJK

# With input
dotnet run ",." "Hello" # H
```

## Build & Test

```bash
make build    # Build project
make test     # Run tests
make publish  # Create cross-platform releases
```

## Brainfuck Operations

| Command | Description              |
|---------|--------------------------|
| `>` `<` | Move pointer             |
| `+` `-` | Increment/decrement cell |
| `.` `,` | Output/input character   |
| `[` `]` | Loop control             |

## Preview

### `dotnet run`

![run.gif](assets/run.gif)

### `dotnet test`

![test.gif](assets/test.gif)