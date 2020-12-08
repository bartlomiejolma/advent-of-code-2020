type Instruction =
    | Nop
    | Acc of int
    | Jmp of int

let convertIntoInstruction (instruction :: number :: rest) =
    instruction
    |> function
    | "nop" -> Nop
    | "jmp" -> Jmp(int number)
    | "acc" -> Acc(int number)

let parseLine (line: string): Instruction =
    line.Split ' '
    |> Seq.toList
    |> convertIntoInstruction

parseLine "nop +0"
parseLine "acc +1"
parseLine "jmp -3"


let getProgram (texInput: string) =
    texInput.Split '\n'
    |> Seq.toList
    |> List.map parseLine

let textInput = "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6"

getProgram textInput

type State = { Accumulator: int; Cursor: int }

let executeInstruction { Accumulator = accumulator; Cursor = cursor } instruction =
    instruction
    |> function
    | Nop ->
        { Accumulator = accumulator
          Cursor = cursor + 1 }
    | Acc change ->
        { Accumulator = accumulator + change
          Cursor = cursor + 1 }
    | Jmp change ->
        { Accumulator = accumulator
          Cursor = cursor + change }

parseLine "nop +0"
|> executeInstruction { Accumulator = 0; Cursor = 0 }

Acc 1
|> executeInstruction { Accumulator = 0; Cursor = 0 }

Jmp -1
|> executeInstruction { Accumulator = 0; Cursor = 0 }


let rec findRepeatingInstruction (state: State) (visitedLines: int Set) (program: Instruction list) =
    let newState =
        executeInstruction state program.[state.Cursor]

    visitedLines.Contains newState.Cursor
    |> function
    | true -> state.Accumulator
    | _ -> findRepeatingInstruction newState (visitedLines.Add newState.Cursor) program

let solve (texInput: string) =
    texInput
    |> getProgram
    |> findRepeatingInstruction { Accumulator = 0; Cursor = 0 } Set.empty

solve textInput

open System.IO

let program = File.ReadAllText("input.txt")

solve program