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

type ShipState =
    { Position: Position
      WaypointPosition: Position }



let executeDirectCommand { East = x; North = y } =
    function
    | North ystep -> { East = x; North = y + ystep }
    | South ystep -> { East = x; North = y - ystep }
    | East xstep -> { East = x + xstep; North = y }
    | West xstep -> { East = x - xstep; North = y }

executeDirectCommand { East = 1; North = 2 } (North 10)

let turnWaypointClockWise { East = x; North = y } turns =
    turns / 90
    |> function
    | 1 -> { East = y; North = - x}
    | 2 -> { East = -x; North = -y }
    | 3 -> { East = -y ; North = x }
    | 4 -> { East = x; North = y }


turnWaypointClockWise {East = 10; North = 1} 90
turnWaypointClockWise {East = 10; North = 1} 180
turnWaypointClockWise {East = 10; North = 1} 270
turnWaypointClockWise {East = 10; North = 1} 360

let turnWaypointCounterClockWise { East = x; North = y } turns =
    turns / 90
    |> function
    | 1 -> { East = -y; North = x}
    | 2 -> { East = -x; North = -y }
    | 3 -> { East = y ; North = -x }
    | 4 -> { East = x; North = y }

let executeBearingCommand waypointPosition =
    function
    | Left turn -> turnWaypointCounterClockWise waypointPosition turn
    | Right turn -> turnWaypointClockWise waypointPosition turn


let executeForwardCommand { Position = { North = shipNorth; East = shipEast };
                            WaypointPosition = { North = waypointNorth; East = waypointEast } }
                          times
                          =

    { North = shipNorth + times * waypointNorth
      East = shipEast + times * waypointEast }

// let executeForwardCommand { Position = shipPos;
//                             WaypointPosition = waypointPos }
//                           times
//                           =

//     shipPos + waypointPos * times

let command = ForwardCommand 10
id (ForwardCommand 10)
1 % 4

5 % 4

// executeForwardCommand
//     { Position = { East = 1; North = 1 }
//       Bearing = E }
//     (ForwardCommand 10)

let executeCommand { Position = position; WaypointPosition = waypointPosition } fullcommand =
    // printfn "%O %O %O" position bearing fullcommand
    fullcommand
    |> function
    | DirectCommand command ->
        { Position = position
          WaypointPosition = executeDirectCommand waypointPosition command }
    | BearingCommand command ->
        { Position = position
          WaypointPosition = executeBearingCommand waypointPosition command }
    | ForwardCommand times ->
        { Position =
              executeForwardCommand
                  { Position = position
                    WaypointPosition = waypointPosition }
                  times
          WaypointPosition = waypointPosition }

// executeCommand
//     { Position = { East = 1; North = 1 }
//       Bearing = E }
//     (ForwardCommand 10)


let manhattanDistance { Position = { East = x; North = y } } = abs x + abs y

let executeInstructions (textInput: string) =
    textInput.Split '\n'
    |> Seq.toList
    |> List.map lineToCommand
    |> List.fold
        executeCommand
           { Position = { East = 0; North = 0 }
             WaypointPosition = { East = 10; North = 1 } }
    |> manhattanDistance

let textInput = "F10
N3
F7
R90
F11"

executeCommand
    { Position = { East = 0; North = 0 }
      WaypointPosition = { East = 10; North = 1 } }
    (ForwardCommand 10)

executeCommand
    { Position = { East = 100; North = 10 }
      WaypointPosition = { East = 10; North = 1 } }
    (DirectCommand(North 3))

executeCommand
    { Position = { East = 100; North = 10 }
      WaypointPosition = { East = 10; North = 4 } }
    (ForwardCommand 7)

executeCommand
    { Position = { East = 170; North = 38 }
      WaypointPosition = { East = 10; North = 4 } }
    (BearingCommand (Right 90))


executeInstructions textInput

open System.IO

let instructionsText = File.ReadAllText("input.txt")

executeInstructions instructionsText
// executeBearingCommand W (Right 90)
