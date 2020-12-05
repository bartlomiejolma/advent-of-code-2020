let text = "FBFBBFFRLR"

let row, column = text.[..6], text.[7..]

let parseRowChar = function
    | 'F' -> '0'
    | 'B' -> '1'


let parseColChar = function
    | 'R' -> '1'
    | 'L' -> '0'

"FBFBBFF"
|> Seq.toList
|> List.map parseRowChar
|> System.String.Concat
|> (+) "0b"
|> int

let parsePart (partParser: char-> char) (part:string) = 
    part
    |> Seq.toList
    |> List.map partParser
    |> System.String.Concat
    |> (+) "0b"
    |> int

let parseRowPart  = parsePart parseRowChar

let parseColPart = parsePart parseColChar
 
parseColPart column

"0b0101100" |> int


let parseSeat (text:string) =
    let row, column = text.[..6], text.[7..]
    let rowNo = parseRowPart row
    let colNo = parseColPart column
    rowNo * 8 + colNo

parseSeat text

parseSeat "BFFFBBFRRR"
parseSeat "FFFBBBFRRR"
parseSeat "BBFFBBFRLL"


let parseSeats (textInput: string) =
    textInput.Split '\n'
    |> Seq.toList
    |> List.map parseSeat
    |> List.sort 

let textInput = "BFFFBBFRRR
FFFBBBFRRR
BBFFBBFRLL"

parseSeats textInput


open System.IO 
let seats = File.ReadAllText("input.txt")

parseSeats seats


let getSeatNumber (prev, next) = prev + 1
let findSeat seats = 
    seats 
    |> List.sort
    |> List.pairwise
    |> List.find (fun (prev, current) -> (current - prev) = 2)
    |> getSeatNumber

[1;4;5;7;8] |> findSeat

seats |> parseSeats |>findSeat