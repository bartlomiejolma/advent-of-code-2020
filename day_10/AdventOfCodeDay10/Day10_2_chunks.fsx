let textInput = "16
10
15
5
1
11
7
19
6
12
4"

let parseTextInput (textInput: string) =
    textInput.Split '\n' |> Seq.map int |> Seq.toList

let rec getAllChains adapters =
    adapters
    |> function
    | [] -> Some 1.0
    | nonEmptyList ->
        nonEmptyList
        |> List.partition (fun x -> x <= 3)
        |> function
        | (first, _) ->
            match first with
            | [] -> None
            | numbersWithinReach ->
                numbersWithinReach
                |> List.map (fun adapterToCheck ->
                    adapters
                    |> List.except [adapterToCheck]
                    |> List.map (fun adapter -> adapter - adapterToCheck)
                    |> List.filter (fun x -> x > 0)
                    |> getAllChains)
                |> List.choose id
                |> List.sum
                |> Option.Some


1
|> Option.Some

[ 1; 2; 3 ] |> List.partition (fun x -> x > 4)

[ 1; 2; 3 ] |> List.except [2]

let solve textInput =
    let adapters = parseTextInput textInput
    let deviceAdapter = (List.max adapters) + 3

    let sorted = 
        [0; deviceAdapter] @ adapters
        |> List.sort

    let indexesOf3diff = 
        sorted
        |> List.pairwise
        |> List.map (fun (prev, next) -> next - prev)
        |> List.indexed
        |> List.filter (fun (_, difference) -> difference = 3)
        |> List.map (fun (index, _) -> index)

    let results = 
        [1..5]
        |> List.map (fun x -> [1..x] |> getAllChains)

    -1::indexesOf3diff
    |> List.pairwise
    |> List.map (fun (start, last) -> last - start - 2)
    |> List.countBy id
    |> List.map (fun (difference, count) -> 
        List.tryItem difference results
        |> function
        | Some result ->
            result 
            |> function
            | Some value -> double(value) ** double(count)
            | None -> 1.0
        | None -> 1.0)
    |> List.reduce (*)


solve textInput

[0; 1; 4; 5; 6; 7; 10; 11; 12; 15; 16; 19; 22]
|> List.length

[0; 1; 4; 5; 6; 7; 10; 11; 12; 15; 16; 19; 22].[1 + 1..5 - 1]

Some 16.0 
|> Option.map ((*) 0.5)

let largerInput = "28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3"

solve largerInput

24 * 3 * 13 * 7

[1..5]
|> List.map (fun x -> [1..x] |> getAllChains)

getAllChains [1;2;3;4;5;6]
getAllChains [1;2;3;4;5]
getAllChains [1;2;3;4]
getAllChains [1;2;3]
getAllChains [1;2]
getAllChains [1]
getAllChains []
open System.IO

let inputFile = File.ReadAllText("input.txt")

solve inputFile
|> printfn "%f" 

282475249.0 * 2.0 ** 15.0