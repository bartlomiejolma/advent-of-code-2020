let textInput = "939
7,13,x,x,59,x,31,19"

let parseInput2 (texInput: string) =
    let _ :: bussesText :: _ = Seq.toList (texInput.Split '\n')
    bussesText.Split ','
    |> Seq.map System.Int32.TryParse
    |> Seq.indexed
    |> Seq.filter (fun (index, (isnumber, number)) -> isnumber)
    |> Seq.map (fun (index, (isnumber, number)) -> (double index, double (number)))
    |> Seq.sortByDescending (fun (index, number) -> number)
    |> Seq.toList

let textInput2 = "939
7,13,19"

let busses = parseInput2 textInput
let (biggestBusOffset, biggestBusSchedule) = busses.Item 0


1442 % 7 = 7 - 0
1442 % 13
1442 % 19

let checkNumber (busses: (double * double) list ) (number: double) =
    busses
    |> List.map (fun (offset, schedule) -> ((if offset = 0.0 then schedule else offset), schedule))
    |> List.forall (fun (busOffset, busSchedule) ->
        number % busSchedule = busSchedule - busOffset)

if 0 =0 then 7 else 1
checkNumber busses 1442.0

[ 1.0 .. 100000.0 ]
|> List.map (fun step ->
    step
    * double (biggestBusSchedule)
    - double (biggestBusOffset))
|> List.find (checkNumber busses)

let solve textInput = 
    let busses = parseInput2 textInput
    let (biggestBusOffset, biggestBusSchedule) = busses.Item 0

    [ 1.0 .. 1000000000.0 ]
    |> List.map (fun step ->
        step
        * double (biggestBusSchedule)
        - double (biggestBusOffset))
    |> List.find (checkNumber busses)


solve textInput

checkNumber busses 910.0

910 % 7
910 % 13
910 % 19

// parseInput textInput

[ 1 .. 10 ] |> List.map ((*) 7)

open System.IO

let scheduleText = File.ReadAllText("input.txt")

solve scheduleText
