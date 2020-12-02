let textInput = "1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc"



let line = "1-3 a: abcde"

type Range = { Min: int; Max: int }

let parseRange (range: string []) =
    { Min = int range.[0]
      Max = int range.[1] }

type Rule = { Range: Range; Letter: char }

let parseRule (rule: string []) =
    let parsedRange = rule.[0].Split '-' |> parseRange
    { Range = parsedRange
      Letter = char rule.[1] }

type Line = { Rule: Rule; Password: string }

let parsePasswordLine (line: string []) =
    let parsedRule = line.[0].Split ' ' |> parseRule
    { Rule = parsedRule
      Password = line.[1] }


let isWithinRange range number =
    number <= range.Max && number >= range.Min

let validatePassword (line: Line) =
    line.Password
    |> String.filter (fun x -> x = line.Rule.Letter)
    |> String.length
    |> isWithinRange line.Rule.Range

let checkLine (line: string) =
    line.Split ':'
    |> parsePasswordLine
    |> validatePassword


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
