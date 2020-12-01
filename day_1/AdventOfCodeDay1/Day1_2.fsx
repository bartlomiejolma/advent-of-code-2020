let input = [ 1721; 979; 366; 299; 675; 1456 ]

let expectedSum = 2020

let sumValues values =
    [ for x in values do
        for y in values do
            for z in values do
                yield x, y, z, x + y + z ]


let tupleMatch (x, y, z, sum) expectedSum = expectedSum = sum



let getProduct (x, y, z, sum) = x * y * z





let findProduct expectedSum values =
    values
    |> sumValues
    |> List.find (fun x -> tupleMatch x expectedSum)
    |> getProduct

findProduct expectedSum input


open System.IO // Name spaces can be opened just as modules

let msg = File.ReadAllText("input.txt")

msg.Split [| '\n' |]
|> Seq.toList
|> List.map (int)
|> findProduct expectedSum

