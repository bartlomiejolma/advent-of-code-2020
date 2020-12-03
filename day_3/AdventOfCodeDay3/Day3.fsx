let textInput = "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#"

type Square = 
    | Tree
    | Open

let getSquare = function
    | '.' -> Open
    | '#' -> Tree
    | _ -> Open

getSquare '#'

let rec lineFormat = function
    | [] -> []
    | head :: tail -> getSquare head :: (lineFormat(tail))

let parseInput (textInput: string) = 
    textInput.Split '\n'
    |> Seq.toList
    |> List.map (Seq.toList >> lineFormat)

parseInput textInput





let adjustHorizontalIndex step maxLength (currentIndex: int) = 
    let newHorizontalIndex = currentIndex + step
    newHorizontalIndex >= maxLength
    |> function 
        | true -> (newHorizontalIndex % maxLength)
        | false -> newHorizontalIndex


let rec move horizontalIndex (squaresList: Square list list)  = 
    match squaresList with
    | [] -> 0
    | _ ->
        let maxLength = (List.length (List.head squaresList))
        let adjustedIndex = adjustHorizontalIndex 3 maxLength horizontalIndex

        squaresList.[0].[adjustedIndex]
        |> function
            | Tree -> 1
            | Open -> 0
        |> (+) (move adjustedIndex (List.tail squaresList) )


parseInput textInput
|> List.tail
|> move 0

open System.IO 

let msg = File.ReadAllText("input.txt")

parseInput msg
|> List.tail
|> move 0


[Open; Tree; Open; Tree; Open; Open; Open; Open; Open; Open; Open; Open; Open; Open; Tree; Open; Open; Open; Tree; Open; Open; Open; Open; Open;Open; Open; Open; Tree; Open; Open; Tree; Open]
|> List.length