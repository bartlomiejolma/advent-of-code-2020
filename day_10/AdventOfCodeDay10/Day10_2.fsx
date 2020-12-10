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

let rec getAllChains (chainsCount: double) adapters =
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
                    |> getAllChains chainsCount)
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

    [ deviceAdapter ] @ adapters
    |> getAllChains 0.0

solve textInput

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


open System.IO

let inputFile = File.ReadAllText("input.txt")

solve inputFile
