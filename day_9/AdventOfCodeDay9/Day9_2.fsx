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




let rec findContiguousSet window number input =
    input
    |> List.windowed window
    |> List.map List.sum
    |> List.tryFindIndex ((=) number)
    |> function
    | Some (index) -> (index, window)
    | _ -> findContiguousSet (window + 1) number input

let solve2 preamble textInput = 
    let (number, _) = solve preamble textInput
    let input = parseInput textInput

    let (index, window) = findContiguousSet 2 number input

    let contiguousSet = input.[index..index+window-1]
    (List.min contiguousSet) + (List.max contiguousSet)

solve2 5 textInput

(parseInput textInput).[5..]

[ 1; 2; 3 ]
|> List.allPairs [1;2;3]


open System.IO

let inputFile = File.ReadAllText("input.txt")

solve2 25 inputFile
