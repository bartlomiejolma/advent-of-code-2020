let asMap (numbers: int list) =
    numbers
    |> List.indexed
    |> List.map (fun (index, number) -> (number, index))
    |> Map

asMap [ 0; 3; 6 ]

let nextNumber lastNumber total (numbers: Map<int, int>) =
    numbers
    |> Map.tryFind (lastNumber)
    |> function
    | Some index -> total - index //numbers.Length - index + 1
    | _ -> 0

nextNumber 6 2 (asMap [ 0; 3 ])

nextNumber 0 3 (asMap [ 0; 3; 6 ])

nextNumber 3 4 (asMap [ 0; 3; 6; 0 ])

nextNumber 3 5 (asMap [ 0; 3; 6; 0; 3 ])

nextNumber 1 6 (asMap [ 0; 3; 6; 0; 3; 1 ])

(asMap [ 3; 6 ]).Add(0, 0)


let rec fillList targetLength lastNumber total (numbers: Map<int, int>) =
    total = targetLength
    |> function
    | true -> lastNumber, numbers
    | _ -> fillList targetLength (nextNumber lastNumber total numbers) (total + 1) (numbers.Add(lastNumber, total))

fillList 8 6 2 (asMap [ 0; 3 ])

let solve target (inputText: string) =
    let input =
        inputText.Split ',' |> Seq.toList |> List.map int

    let lastAdded = input.[input.Length - 1]
    let numbers = asMap input.[ 0 .. input.Length - 2 ]
    fillList (target - 1) lastAdded (input.Length - 1) numbers

solve 2020 "0,3,6"

solve 2020 "9,19,1,6,0,5,4"

solve 30000000 "9,19,1,6,0,5,4"
