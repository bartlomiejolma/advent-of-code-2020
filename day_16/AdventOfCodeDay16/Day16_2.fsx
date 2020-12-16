open System

type Range = { LB: int; UB: int }
type Rule = { Text: string; Ranges: Range list }

let parseRule (ruleText: string) =
    let text :: rangesText :: _ = ruleText.Split ':' |> Seq.toList

    let ranges =
        rangesText.Split([| "or" |], StringSplitOptions.None)
        |> Seq.toList
        |> List.map (fun range ->
            let lb :: ub :: _ = range.Trim().Split('-') |> Seq.toList
            { LB = int lb; UB = int ub })

    { Text = text; Ranges = ranges }

parseRule "class: 38-291 or 304-968"

let validateRule number { Ranges = rule } =
    rule
    |> List.exists (fun { LB = lb; UB = ub } -> number <= ub && number >= lb)


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

let parseTicket (ticketText: string) =
    ticketText.Split ',' |> Seq.toList |> List.map int

parseTicket "7,3,47"

let validateTicket rules ticket =
    ticket
    |> List.map (fun number -> rules |> List.filter (validateRule number))

let numbers = [ 1; 2; 3; 4 ]

let index = numbers |> List.find ((=) 2)

numbers.[1..index - 1]
numbers.[0..index - 2] @ numbers.[index..]

let rec foo indexes ticket rules =
    rules
    |> List.length
    |> function
    | 0 -> indexes |> List.rev
    | _ ->
        ticket
        |> List.tryFindIndex (fun ticketRules ->
            ticketRules
            |> function
            | [ _ ] -> true
            | _ -> false)
        |> function
        | Some index ->
            let [ singleRule ] = ticket.Item index
            let filteredRules = rules |> List.filter ((<>) singleRule)
            foo (index :: indexes) (ticket.[0..index - 2] @ ticket.[index..]) filteredRules

        | _ ->
            let index = 0
            let [ singleRule ] = ticket.Item index
            let filteredRules = rules |> List.filter ((<>) singleRule)
            foo (index :: indexes) (ticket.[0..index - 2] @ ticket.[index..]) filteredRules

let solve (textInput: string) =
    let rulesText :: _ :: ticketsText :: _ =
        textInput.Split([| "\n\n" |], StringSplitOptions.None)
        |> Seq.toList

    let rules =
        rulesText.Split '\n'
        |> Seq.toList
        |> List.map parseRule

    let tickets =
        ticketsText.Split '\n'
        |> Seq.toList
        |> List.tail
        |> List.map parseTicket

    let validTickets =
        tickets
        |> List.map (validateTicket rules)
        |> List.filter (fun rules ->
            rules
            |> List.map (List.length)
            |> List.forall ((<>) 0))

    // validTickets
    // |> List.collect (fun rules ->
    //     rules
    //     |> List.indexed
    //     |> List.map (fun (index, mathches) -> (index, mathches |> List.length, mathches))
    //     |> List.filter (fun (_, matchesCount, _) -> matchesCount = 2)
    //     |> List.collect (fun (index, _, (singleMatch :: _)) -> [(index, singleMatch)]))

    rules
    |> List.map (fun rule ->
        validTickets
        |> List.collect (fun ticket ->
            ticket
            |> List.indexed
            |> List.choose (fun (index, ticketRules) ->
                ticketRules
                |> List.tryFind ((=) rule)
                |> function
                | Some _ -> None
                | _ -> Some index)))
    |> List.sortByDescending List.length


solve textInput

open System.IO

let fullTicketsInput = File.ReadAllText("input.txt")

solve fullTicketsInput

let rulesText :: yourText :: ticketsText :: _ =
    fullTicketsInput.Split([| "\n\n" |], StringSplitOptions.None)
    |> Seq.toList

let rules =
    rulesText.Split '\n'
    |> Seq.toList
    |> List.map parseRule

let tickets =
    ticketsText.Split '\n'
    |> Seq.toList
    |> List.tail
    |> List.map parseTicket

let validTickets =
    tickets
    |> List.map (validateTicket rules)
    |> List.filter (fun rules ->
        rules
        |> List.map (List.length)
        |> List.forall ((<>) 0))

let uniquePositions = 
    rules
    |> List.map (fun rule ->
        validTickets
        |> List.collect (fun ticket ->
            ticket
            |> List.indexed
            |> List.choose (fun (index, ticketRules) ->
                ticketRules
                |> List.tryFind ((=) rule)
                |> function
                | Some _ -> None
                | _ -> Some index)))
    |> List.indexed
    |> List.sortByDescending (fun (_, notMatchinPositions) -> notMatchinPositions |>List.length)

rules
|> List.length

[0..19]
|> List.except [4; 8; 0; 17; 15; 3; 10; 7; 2; 5; 18; 12; 1; 13; 6; 11; 9; 14; 16]


let rec getPositions availablePositions setPositions rules = 
    rules
    |> function
    | [] -> setPositions
    | (ruleIndex, longestRule)::otherRules -> 
        let rulePosition = 
            availablePositions
            |> List.except longestRule
            |> function
            | [singleMatch] -> singleMatch

        getPositions (availablePositions |> List.except [rulePosition]) ((ruleIndex, rulePosition) :: setPositions) otherRules 

let rulesPositions = getPositions [0..rules.Length - 1] [] uniquePositions

let departurePositions = 
    rules
    |> List.indexed
    |> List.filter (fun (index, {Text=text}) -> text.Contains "departure")
    |> List.map ((fun (index, _) -> 
        rulesPositions
        |> List.find (fun (ruleIndex, _) -> index=ruleIndex))>> (fun (_, rulePosition) -> rulePosition))

let _::yourTicketText::_  = yourText.Split '\n' |> Seq.toList

let yourTicket = parseTicket yourTicketText

departurePositions
|> List.map (fun position -> uint64(yourTicket.[position]))
|> List.reduce (*)