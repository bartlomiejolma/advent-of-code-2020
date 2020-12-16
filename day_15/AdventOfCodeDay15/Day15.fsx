

let nextNumber (numbers: int list) =
    numbers
    |> List.tail
    |> List.tryFindIndex ((=) (numbers |> List.head))
    |> function
    | Some index -> index + 1//numbers.Length - index + 1
    | _ -> 0

nextNumber [6;3;0]

nextNumber [0;6;3;0]

nextNumber [3;0;6;3;0]

nextNumber [3;3;0;6;3;0]

nextNumber [1;3;3;0;6;3;0]

nextNumber [0;1;3;3;0;6;3;0]

let rec fillList targetLength (numbers: int list) = 
    numbers.Length = targetLength
    |> function
    | true -> numbers
    | _ -> fillList targetLength ((nextNumber numbers)::numbers)

fillList 2020 [6;3;0]

let solve target (inputText:string) = 
    inputText.Split ','
    |> Seq.toList
    |> List.rev
    |> List.map int
    |> fillList target
    |> List.head

solve 2020 "9,19,1,6,0,5,4"

solve 30000000 "9,19,1,6,0,5,4"
