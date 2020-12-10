let textInput = "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576"

let parseInput (textInput: string) =
    textInput.Split '\n' |> Seq.toList |> List.map double



let rec pairs numbers =
    match numbers with
    | [] -> []
    | head :: tail ->
        tail
        |> List.map (fun x -> [ head; x ])
        |> (@) (pairs tail)

[ 1; 2; 3 ]
|> pairs
|> List.map List.sum

|> List.pairwise



(parseInput textInput)
|> List.windowed 5
|> List.map (pairs >> (List.map List.sum))

let solve preamble textInput =
    let input = parseInput textInput

    input.[..input.Length - 2]
    |> List.windowed preamble
    |> List.map (pairs >> (List.map List.sum))
    |> List.zip input.[preamble..]
    |> List.find (fun (elementToFind, sums) -> (not) (List.contains elementToFind sums))

solve 5 textInput


(parseInput textInput).[5..]

[ 1; 2; 3 ]
|> List.allPairs [1;2;3]


open System.IO

let input = File.ReadAllText("input.txt")

solve 25 input
