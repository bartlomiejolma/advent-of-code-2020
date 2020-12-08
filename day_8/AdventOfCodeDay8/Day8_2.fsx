type Instruction =
    | Nop of int
    | Acc of int
    | Jmp of int

let convertIntoInstruction (instruction :: number :: rest) =
    instruction
    |> function
    | "nop" -> Nop(int number)
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
    | Nop _ ->
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


let rec executeProgram (state: State) (visitedLines: int Set) (program: Instruction list) =
    state.Cursor >= List.length program
    |> function
    | true -> Some state.Accumulator
    | false ->
        let newState =
            executeInstruction state program.[state.Cursor]

        visitedLines.Contains newState.Cursor
        |> function
        | true -> None
        | _ -> executeProgram newState (visitedLines.Add newState.Cursor) program

let testProgram (program: Instruction list) =
    program
    |> executeProgram { Accumulator = 0; Cursor = 0 } Set.empty
    |> function
    | Some _ -> true
    | _ -> false

Nop 5
|> function
| Nop value -> value
| Jmp alternative -> alternative
| Acc alternative -> alternative

let extractValue =
    function
    | Nop value -> value
    | Jmp value -> value
    | Acc value -> value


let getProgamWithSwapped instructionToSwap (program: Instruction list) (index: int, originalInstruction: Instruction) =
    program.[..(index - 1)]
    @ [ instructionToSwap (extractValue originalInstruction) ]
    @ program.[(index + 1)..]


let isNop = function
    | Nop _ -> true
    | _ -> false

let isJmp = function
    | Jmp _ -> true
    | _ -> false

let getProgramsWithSwapped fitlerFunction instructionToSubstitute program =
    program
    |> List.indexed
    |> List.filter (fun (_, instruction) -> fitlerFunction instruction)
    |> List.map (getProgamWithSwapped instructionToSubstitute program)

let getAllAlternativePrograms program =
    let programsWithJmpSwapped = getProgramsWithSwapped (isJmp) (Nop) (program)

    let programsWithNopSwapped = getProgramsWithSwapped (isNop) (Jmp) (program)

    programsWithJmpSwapped @ programsWithNopSwapped
    

let findInstructionToChange programText =
    programText
    |> getProgram
    |> getAllAlternativePrograms
    |> List.find (testProgram)
    |> executeProgram { Accumulator = 0; Cursor = 0 } Set.empty

findInstructionToChange textInput


open System.IO

let programText = File.ReadAllText("input.txt")

findInstructionToChange programText
