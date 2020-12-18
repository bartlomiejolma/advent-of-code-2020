type Operation =
    | Multiplication
    | Addition

type Symbol =
    | Number of int
    | Operation of Operation
    | OpeningBracket
    | ClosingBracket

("((4".Split '(').Length

let parseNumber (other: string) =
    let otherWithOpenning = other.Split '('
    otherWithOpenning
    |> Seq.toList
    |> function
    | [ single ] ->
        let singleWithClosing = single.Split ')'
        singleWithClosing
        |> Seq.toList
        |> function
        | [ single ] -> [ Number(System.Int32.Parse single) ]
        | single :: tail ->
            [ Number(System.Int32.Parse single) ]
            @ (tail |> List.map (fun _ -> ClosingBracket))
    | other ->
        (other.[0..other.Length - 2]
         |> List.map (fun _ -> OpeningBracket))
        @ [ Number(System.Int32.Parse other.[other.Length - 1]) ]

let parseSymbol =
    function
    | "+" -> [ (Operation Addition) ]
    | "*" -> [ (Operation Multiplication) ]
    | other -> parseNumber other

let parseLine (textInput: string) =
    textInput.Split ' '
    |> Seq.toList
    |> List.collect parseSymbol

parseLine "1 + (2 * 3) + (4 * (5 + 6))"


let rec executeLine total =
    function
    | [] -> total
    | head :: tail ->
        head
        |> function
        | Operation Addition -> total + (executeLine total tail)
        | Operation Multiplication -> total * (executeLine total tail)
        | Number number -> number
        | OpeningBracket -> (executeLine total tail)
        | ClosingBracket -> total

parseLine "1 + (2 * 3) + (4 * (5 + 6))"
|> executeLine 0


open System.Text.RegularExpressions


Regex.Match("1 + (2 * 3) + (4 * (5 + 6))", "\((.*)\)")

let aMatch = Regex.Match("2 * 3 + 1", "(\d [+*] \d)")

let parseMatch (aMatch: string) =
    aMatch.Trim().Split ' '
    |> Seq.toList
    |> function
    | [ left; operator; right ] ->
        match operator with
        | "*" -> string (double (left) * double (right))
        | "+" -> string (double (left) + double (right))
    | _ -> aMatch

parseMatch "4 * 11"

let rec parseContent (content: string) =
    Regex.Split(content, "(\d+ [+,*] \d+)")
    |> Seq.length
    |> function
    | 1 -> content
    | _ ->
        let aMatch = Regex.Match(content, "(\d+ [+,*] \d+)")
        aMatch.Index 
        |> function
        | 0 ->
            parseContent
                (content.[0..aMatch.Index - 1]
                 + (parseMatch aMatch.Value)
                 + content.[aMatch.Index + aMatch.Length..])
        | _ -> content

parseContent "2 * 3 + 1"

parseContent "1 +"

parseContent "1 + 6 * "

Regex.Match("1 + ", "(\d [+*] \d)")

let parseSplit (split: string) =
    (split.Contains("(") || split.Contains(")"))
    |> function
    | false -> parseContent split
    | true -> split

parseContent "5 + 6"

let rec parseEquation (equation: string) =
    let aMatch = Regex.Match(equation, "[+*]")
    aMatch.Success
    |> function
    | false -> equation
    | true ->
        Regex.Split(equation, "\(([0-9 +*]*)\)")
        |> Seq.toList
        |> List.map parseSplit
        |> List.reduce (+)
        |> parseEquation


parseEquation "1 + (2 * 3) + (4 * (5 + 6))"

parseEquation "2 * 3 + (4 * 5)"
parseEquation "1 + 6 + (4 * 11)"
parseEquation "5 + (8 * 3 + 9 + 3 * 4 * 3)"
parseEquation "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"
parseContent "4 * 11"
parseSplit "1 + "

parseEquation "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"

Regex.Split("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", "\(([1-9, +, * ]*)\)")
|> Seq.toList
|> List.map parseSplit

Regex.Split("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))", "\(([1-9, +, * ]*)\)")

Regex.Match("1 + ", "\d+ [+,*] \d+")

Regex.Split("5 + 6 * 2", "(\d [+*] \d)")

Regex.Split("1 +", "(\d [+*] \d)")

Regex.Split("5 + 6", "(\d [+*] \d)")
|> Array.map parseMatch
|> Array.reduce (+)

" + 1".Trim()
parseMatch " + 1"


open System.IO

let equations = File.ReadAllText("input.txt")

equations.Split '\n'
|> Seq.toList
|> List.sumBy (parseEquation >> (double))
|> printfn "%f"

Regex.Split("(9 + 36 * 210 + 7) + 2 + 4 * 2 + 3 * 13", "\(([0-9 +*]*)\)")
