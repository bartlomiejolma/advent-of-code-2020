let textInput = "1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc"



let line = "1-3 a: abcde"

let parsePositions (range: string []) = [ int range.[0]; int range.[1] ]

type Rule = { Positions: List<int>; Letter: char }

let parseRule (rule: string []) =
    let parsedPositions = rule.[0].Split '-' |> parsePositions
    { Positions = parsedPositions
      Letter = char rule.[1] }

type Line = { Rule: Rule; Password: string }

let parsePasswordLine (line: string []) =
    let parsedRule = line.[0].Split ' ' |> parseRule
    { Rule = parsedRule
      Password = line.[1] }


let validatePassword (line: Line) =
    line.Rule.Positions
    |> List.map((fun index -> line.Password.[index]) >> (fun character -> character = line.Rule.Letter))
    |> List.filter id
    |> List.length
    |> (=) 1

let checkLine (line: string) =
    line.Split ':'
    |> parsePasswordLine
    |> validatePassword


checkLine line

let line2 = "2-9 c: ccccccccc"

checkLine line2

let numberOfValidPasswords (textInput: string) =
    textInput.Split '\n'
    |> Seq.toList
    |> List.map checkLine
    |> List.filter id
    |> List.length

numberOfValidPasswords textInput

open System.IO // Name spaces can be opened just as modules

let msg = File.ReadAllText("input.txt")

numberOfValidPasswords msg
