

let parseInput (texInput: string) = 
    let timestampText::bussesText::_ = Seq.toList (texInput.Split '\n')
    let busses = 
        bussesText.Split ','
        |> Seq.filter (fun time -> time <> "x")
        |> Seq.map int
        |> Seq.toList

    (int timestampText, busses)

939 / 7 
|> (+) 1
|> (*) 7
|> (-) -939

let findWaitTime timestamp id =
    double(timestamp) / double(id)
    |> ceil
    |> int
    |> (*) id
    |> (-) timestamp
    |> (*) -1

findWaitTime 939 7
findWaitTime 939 59
939 % 59

let findNearest (timestamp, busses) = 
    // let bussesWaitTime =
    busses
    |> List.map (findWaitTime timestamp)
    |> List.indexed
    |> List.minBy (fun (_, waitTime) -> waitTime)
    |> fun (index, waitTime) -> (busses.Item index, waitTime)
    
    // let minBusWaitTime = 
    //     bussesWaitTime
    //     |> List.min

    // bussesWaitTime
    // |> List.find


let solve scheduleText =
    let schedule =
        scheduleText
        |> parseInput
    
    let (busId, timeToDeparture) = findNearest schedule

    timeToDeparture * busId

let texInput = "939
7,13,x,x,59,x,31,19"


solve texInput

open System.IO

let scheduleText = File.ReadAllText("input.txt")

solve scheduleText