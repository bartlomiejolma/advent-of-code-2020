// Learn more about F# at http://fsharp.org

open System


let sumValues values =
    [ for x in values do
        for y in values do
            yield x, y, x + y ]


let tupleMatch (x, y, sum) expectedSum = expectedSum = sum

let getProduct (x, y, sum) = x * y


let findProduct expectedSum values  =
    values
    |> sumValues
    |> List.find (fun x -> tupleMatch x expectedSum)
    |> getProduct

let expectedSum = 2020

open System.IO 

[<EntryPoint>]
let main argv =
    let msg = File.ReadAllText("input.txt")

    msg.Split [|'\n'|]
    |> Seq.toList
    |> List.map(int)
    |> findProduct expectedSum
    |> printf "Result %d"
    0 // return an integer exit code
