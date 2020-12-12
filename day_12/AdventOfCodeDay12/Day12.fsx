let line = "N12"

type DirectCommand =
    | North of int
    | South of int
    | East of int
    | West of int

type BearingCommand =
    | Left of int
    | Right of int

type Command =
    | DirectCommand of DirectCommand
    | ForwardCommand of int
    | BearingCommand of BearingCommand

line.[1..]

let lineToCommand (line: string): Command =
    line.[0]
    |> function
    | 'N' -> DirectCommand(North(int line.[1..]))
    | 'S' -> DirectCommand(South(int line.[1..]))
    | 'E' -> DirectCommand(East(int line.[1..]))
    | 'W' -> DirectCommand(West(int line.[1..]))
    | 'F' -> ForwardCommand(int line.[1..])
    | 'L' -> BearingCommand(Left(int line.[1..]))
    | 'R' -> BearingCommand(Right(int line.[1..]))

lineToCommand line

type Position = { East: int; North: int }

type Bearing =
    | N
    | S
    | E
    | W

type State =
    { Position: Position
      Bearing: Bearing }

let executeDirectCommand { East = x; North = y } =
    function
    | North ystep -> { East = x; North = y + ystep }
    | South ystep -> { East = x; North = y - ystep }
    | East xstep -> { East = x + xstep; North = y }
    | West xstep -> { East = x - xstep; North = y }

executeDirectCommand { East = 1; North = 2 } (North 10)


let executeBearingCommand bearing command =
    let bearings = [ N; E; S; W ]
    let currentBearingIndex = List.findIndex ((=) bearing) bearings
    command
    |> function
    | Left turn -> bearings.Item((currentBearingIndex - (turn / 90) + 4) % 4)
    | Right turn -> bearings.Item((currentBearingIndex + (turn / 90)) % 4)


let executeForwardCommand { Position = position; Bearing = bearing } ((ForwardCommand value)) =
    bearing
    |> function
    | N -> executeDirectCommand position (North value)
    | S -> executeDirectCommand position (South value)
    | E -> executeDirectCommand position (East value)
    | W -> executeDirectCommand position (West value)

let command = ForwardCommand 10
id (ForwardCommand 10)
executeBearingCommand E (Left 360)
1 % 4

5 % 4

executeForwardCommand
    { Position = { East = 1; North = 1 }
      Bearing = E }
    (ForwardCommand 10)

let executeCommand { Position = position; Bearing = bearing } fullcommand =
    // printfn "%O %O %O" position bearing fullcommand
    fullcommand
    |> function
    | DirectCommand command ->
        { Position = executeDirectCommand position command
          Bearing = bearing }
    | BearingCommand command ->
        { Position = position
          Bearing = executeBearingCommand bearing command }
    | ForwardCommand value ->
        { Position =
              executeForwardCommand
                  { Position = position
                    Bearing = bearing }
                  (ForwardCommand value)
          Bearing = bearing }

executeCommand     { Position = { East = 1; North = 1 }; Bearing = E } (ForwardCommand 10)


let manhattanDistance {Position={ East = x; North = y }} = abs x + abs y

let executeInstructions (textInput: string) =
    textInput.Split '\n'
    |> Seq.toList
    |> List.map lineToCommand
    |> List.fold executeCommand  { Position = { East = 0; North = 0 }; Bearing = E }
    |> manhattanDistance

let textInput = "F10
N3
F7
R90
F11"

executeCommand { Position = { East = 0; North = 0 }; Bearing = E } (ForwardCommand 10)
executeCommand {Position = {East = 10;
                              North = 0;};
                  Bearing = E;} (DirectCommand(North 3))


executeInstructions textInput

open System.IO

let instructionsText = File.ReadAllText("input.txt")

executeInstructions instructionsText

executeBearingCommand W (Right 90)
