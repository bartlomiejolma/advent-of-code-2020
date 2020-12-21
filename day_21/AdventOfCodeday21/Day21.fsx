let lineText =
    "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)"

open System

let parseLine (lineText: string) =
    let ingredientsText :: alergensText :: _ =
        lineText.Split([| " (contains " |], StringSplitOptions.None)
        |> Seq.toList



    let ingredients = ingredientsText.Split ' ' |> Seq.toList

    let alergens =
        alergensText.Replace(")", "").Split ','
        |> Seq.toList

    alergens
    |> List.map (fun alergen -> (alergen.Trim(), ingredients))

parseLine lineText


let multilineText = """mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)"""

let solve (multilineText:string) =
    let inputLines =
        multilineText.Split '\n'
        |> Seq.toList
        |> List.map parseLine

    let alergenNames =
        inputLines
        |> List.collect id
        |> List.groupBy (fun (alergen, _) -> alergen)
        |> List.map (fun (alergen, lines) ->
            lines
            |> List.map (fun (_, ingredients) -> Set.ofList ingredients)
            |> Set.intersectMany)
        |> Set.unionMany

    inputLines
    |> List.collect
        (List.head
         >> fun (_, ingredients) -> ingredients)
    |> List.filter (alergenNames.Contains >> not)
    |> List.countBy id
    |> List.sumBy (fun (_, count) -> count)

solve multilineText


open System.IO

let recepies = File.ReadAllText("input.txt")

solve recepies