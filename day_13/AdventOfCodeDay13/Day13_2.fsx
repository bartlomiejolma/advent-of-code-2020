
let textInput = "939
7,13,x,x,59,x,31,19"

let parseInput2 (texInput: string) = 
    let _::bussesText::_ = Seq.toList (texInput.Split '\n')
    bussesText.Split ','
    |> Seq.map System.Int32.TryParse
    |> Seq.indexed
    |> Seq.filter (fun (index, (isnumber, number)) -> isnumber)
    |> Seq.map (fun (index, (isnumber, number)) -> (index, number))
    |> Seq.sortBy (fun (index, number) -> number)
    |> Seq.map ((fun (index, number) -> 
        [1..100] |> List.map (fun multiplayer -> number * multiplayer - index)) >> Set.ofList)

    |> Set.intersectMany

let textInput2 = "939
7,13"
parseInput2 textInput2

let parseInput (texInput: string) = 
    let _::bussesText::_ = Seq.toList (texInput.Split '\n')
    bussesText.Split ','
    |> Seq.map System.Int32.TryParse
    |> Seq.indexed
    |> Seq.filter (fun (index, (isnumber, number)) -> isnumber)
    |> Seq.map (fun (index, (isnumber, number)) -> (index, number))
    |> Seq.sortBy (fun (index, number) -> number)
    |> Seq.map ((fun (index, number) -> 
        [1..100000000] |> List.map (fun multiplayer -> number * multiplayer - index)) >> Set.ofList)

    |> Set.intersectMany

// parseInput textInput

[1..10] |> List.map ((*) 7)

open System.IO

let scheduleText = File.ReadAllText("input.txt")

parseInput scheduleText