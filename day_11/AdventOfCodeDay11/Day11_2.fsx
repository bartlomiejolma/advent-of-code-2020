let textInput = "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL"

type Seat =
    | Empty
    | Occupied
    | Floor

let parseLine (lineText:string) =
    lineText
    |> Seq.map (fun x ->
        match x with 
        | 'L' -> Empty
        | '.' -> Floor
        | '#' -> Occupied)
    |> Seq.toArray

parseLine "L.LL.LL.LL"


let parseInput (textInput:string) = 
    textInput.Split '\n'
    |> Seq.map parseLine
    |> Seq.toArray

let seats = parseInput textInput

[-1..1]
let getNeighbourIndexes dist = 
    let range = [-dist..dist]
    [|for x in range do
        for y in range do
            yield (x, y)|]
    |> Array.filter (fun (x, y) -> not ((x = 0) && (y = 0)) )

getNeighbourIndexes 1

let rec getNeighbour multiplayer seats baseX baseY (neighbourX, neighbourY) =
    seats
    |> Array.tryItem (multiplayer * neighbourX + baseX)
    |> function
    | Some row -> 
        row
        |> Array.tryItem (multiplayer * neighbourY + baseY)
        |> function
        | Some neighbour -> 
            match neighbour with 
            | Floor -> getNeighbour (multiplayer + 1) seats baseX baseY (neighbourX, neighbourY)
            | _ -> Some neighbour
        | _ -> None
    | _ -> None

let rec getNeighbours neigbourIndexes seats baseX baseY= 
    neigbourIndexes
    |> Array.choose (getNeighbour 1 seats baseX baseY)


let neigbourIndexes = getNeighbourIndexes 1
getNeighbours neigbourIndexes seats 0 0


let emptyRules neigbours =
    neigbours
    |> Array.forall (fun seat -> 
        match seat with
        | Occupied -> false
        | _ -> true)
    |> function
    | true -> Occupied
    | false -> Empty

let occupiedRules neigbours =
    neigbours
    |> Array.filter (fun seat -> seat = Occupied)
    |> Array.length
    |> (<) 4
    |> function
    | true -> Empty
    | false -> Occupied

2
|> (<) 3

3
|> (<) 3

4
|> (<)3
let seatRules neigbours seat= 
    match seat with
    | Floor -> Floor
    | Empty -> emptyRules neigbours
    | Occupied -> occupiedRules neigbours

seatRules (getNeighbours neigbourIndexes seats 0 0) seats.[0].[0]

let singleRun neigbourIndexes seats = 
    seats
    |> Array.indexed
    |> Array.map (fun (xIndex, row) -> 
        row
        |> Array.indexed
        |> Array.map (fun (yindex, seat) -> 
            seatRules (getNeighbours neigbourIndexes seats xIndex yindex) seat))

singleRun neigbourIndexes seats
|> singleRun neigbourIndexes
|> singleRun neigbourIndexes

seatRules [|Floor; Occupied; Occupied|] Occupied

occupiedRules [|Floor; Occupied; Occupied|]

let sumOccupied seats = 
    seats
    |> Array.sumBy (fun row -> 
        row
        |> Array.sumBy (fun seat ->
            match seat with
            | Occupied -> 1
            | _ -> 0)
    )
let rec runUntilConverges neigbourIndexes seats =
    let newSeats = singleRun neigbourIndexes seats
    newSeats = seats
    |> function
    | false -> runUntilConverges neigbourIndexes newSeats
    | true -> sumOccupied seats

runUntilConverges neigbourIndexes seats

let solve textInput =
    let seats = parseInput textInput
    let neigbourIndexes = getNeighbourIndexes 1

    runUntilConverges neigbourIndexes seats

solve textInput

open System.IO

let inputFile = File.ReadAllText("input.txt")

solve inputFile
