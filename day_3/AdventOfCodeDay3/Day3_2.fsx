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


let rec move horizontalStep verticalStep horizontalIndex (squaresList: Square list list)  = 
    match verticalStep > (List.length squaresList) with
    | true -> 0
    | _ ->
        let maxLength = (List.length (List.head squaresList))
        let adjustedIndex = adjustHorizontalIndex horizontalStep maxLength horizontalIndex

        squaresList.[verticalStep - 1].[adjustedIndex]
        |> function
            | Tree -> 1
            | Open -> 0
        |> (+) (move horizontalStep verticalStep adjustedIndex (List.skip verticalStep squaresList) )


parseInput textInput
|> List.tail
|> move 3 1 0

parseInput textInput
|> List.tail
|> move 1 1 0

parseInput textInput
|> List.tail
|> move 5 1 0

let countTrees textInput horizontalSlope verticalSlope = 
    parseInput textInput
    |> List.tail
    |> move horizontalSlope verticalSlope 0

countTrees textInput 7 1

countTrees textInput 1 2

let slopes = [
    (1, 1)
    (3, 1)
    (5, 1)
    (7, 1)
    (1, 2)
]

let multipliedOptions textInput slopes = 

    slopes
    |> List.map ((fun (horizontalSlope, verticalSlope) -> countTrees textInput horizontalSlope verticalSlope) >> double)
    |> List.reduce (*)

multipliedOptions textInput slopes

open System.IO 

let msg = File.ReadAllText("input.txt")


multipliedOptions msg slopes