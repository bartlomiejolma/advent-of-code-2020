open System

type Range = { LB: int; UB: int }

let parseRule (ruleText: string) =
    let _ :: ranges :: _ = ruleText.Split ':' |> Seq.toList

    ranges.Split([| "or" |], StringSplitOptions.None)
    |> Seq.toList
    |> List.map (fun range ->
        let lb :: ub :: _ = range.Trim().Split('-') |> Seq.toList
        { LB = int lb; UB = int ub })

parseRule "class: 38-291 or 304-968"

let validateRule number rule =
    rule
    |> List.exists (fun {LB=lb; UB=ub} -> number <= ub && number >= lb)


parseRule "class: 38-291 or 304-968"
|> validateRule 292

let textInput = "class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12"

let parseTicket (ticketText:string) = 
    ticketText.Split ',' |> Seq.toList |> List.map int

parseTicket "7,3,47"

let validateTicket rules ticket =
    ticket
    |> List.choose (fun number ->
        rules
        |> List.exists (validateRule number)
        |> function
        | true -> None
        | false -> Some number)

let solve (textInput: string) = 
    let rulesText::_::ticketsText::_ = textInput.Split([| "\n\n" |], StringSplitOptions.None) |> Seq.toList

    let rules = 
        rulesText.Split '\n'
        |> Seq.toList
        |> List.map parseRule

    let tickets = 
        ticketsText.Split '\n'
        |> Seq.toList
        |> List.tail
        |> List.map parseTicket

    tickets
    |> List.collect (validateTicket rules)
    |> List.sum

solve textInput

open System.IO

let fullTicketsInput = File.ReadAllText("input.txt")

solve fullTicketsInput