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

let parseTextInput (textInput:string) = 
    textInput.Split '\n'
    |> Seq.map int
    |> Seq.toList

let solve textInput = 
    let adapters = parseTextInput textInput
    let deviceAdapter = (List.max adapters) + 3

    let counts = 
        [0; deviceAdapter] @ adapters
        |> List.sort
        |> List.pairwise
        |> List.map (fun (prev, next) -> next - prev)
        |> List.countBy id

    let threes = 
        counts
        |> List.tryFind (fun (diff, _) -> diff = 3)
        |> function
        | Some (_, count) -> count

    let ones = 
        counts
        |> List.tryFind (fun (diff, _) -> diff = 1)
        |> function
        | Some (_, count) -> count

    threes * ones


solve textInput

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