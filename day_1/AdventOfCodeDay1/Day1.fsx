let input = [ 1721; 979; 366; 299; 675; 1456 ]

let expectedSum = 2020

let sumValues values =
    [ for x in values do
        for y in values do
            yield x, y, x + y ]

sumValues input

let tupleMatch (x, y, sum) expectedSum = expectedSum = sum

let findElementsWhichSum expectedSum values  =
    values
    |> sumValues
    |> List.find (fun x -> tupleMatch x expectedSum)

findElementsWhichSum expectedSum input 


let getProduct (x, y, sum) = x * y

let findProduct expectedSum values  =
    values
    |> findElementsWhichSum expectedSum
    |> getProduct

findProduct expectedSum input 




let findProduct' expectedSum values  =
    values
    |> sumValues
    |> List.find (fun x -> tupleMatch x expectedSum)
    |> getProduct

findProduct' expectedSum input 


open System.IO // Name spaces can be opened just as modules
let msg = File.ReadAllText("input.txt")

msg.Split [|'\n'|]
|> Seq.toList
|> List.map(int)
|> findProduct' expectedSum



let someString = ["123"; "5"] 
let someInt = someString |> List.map(int) // someInt now contains 123 int value