type Cell = { X: int; Y: int; Z: int }

let neighbourIndexes =
    [| for x in [ -1 .. 1 ] do
        for y in [ -1 .. 1 ] do
            for z in [ -1 .. 1 ] do
                yield { X = x; Y = y; Z = z } |]
    |> Array.except [ { X = 0; Y = 0; Z = 0 } ]

neighbourIndexes |> Array.length

let getNeighbours { X = xC; Y = yC; Z = zC } =
    neighbourIndexes
    |> Array.map (fun { X = x; Y = y; Z = z } -> { X = x + xC; Y = y + yC; Z = z + zC })

[| 1 .. 3 |] |> Array.contains 2

let generation cells =
    let cellsWithNeighbours =
        cells
        |> Array.collect getNeighbours
        |> Array.countBy id

    cellsWithNeighbours
    |> Array.filter (fun (_, count) -> count = 2)
    |> Array.map (fun (cell, count) -> (cell, (if (cells |> Array.contains cell) then count + 1 else count)))
    |> Array.append cellsWithNeighbours
    |> Array.filter (fun (_, count) -> count = 3)
    |> Array.map (fun (cell, co_unt) -> cell)

generation (getNeighbours { X = 0; Y = 0; Z = 0 })

getNeighbours { X = 0; Y = 0; Z = 0 }
|> Array.collect getNeighbours
|> Array.countBy id
// |> Array.length
// |> Array.head
// |> Array.map (fun (cell, occurences) -> (cell, occurences |> Array.length))

{ X = 0; Y = 0; Z = 1 } |> id


let textInput = ".#.
..#
###"

textInput.Split '\n'
|> Seq.toList
|> List.indexed
|> List.collect (fun (x, chars) -> 
    chars
    |> Seq.toList
    |> List.indexed
    |> List.filter (fun (_, char) -> char = '#')
    |> List.map (fun (y, _) -> {X=x;Y=y;Z=0}))
|> List.toArray
|> generation
|> generation
|> generation
|> generation
|> generation
|> generation
|> Array.length



let puzzleInput = "##...#.#
####.#.#
#...####
..#.#.#.
####.#..
#.#.#..#
.####.##
..#...##"

puzzleInput.Split '\n'
|> Seq.toList
|> List.indexed
|> List.collect (fun (x, chars) -> 
    chars
    |> Seq.toList
    |> List.indexed
    |> List.filter (fun (_, char) -> char = '#')
    |> List.map (fun (y, _) -> {X=x;Y=y;Z=0}))
|> List.toArray
|> generation
|> generation
|> generation
|> generation
|> generation
|> generation
|> Array.length