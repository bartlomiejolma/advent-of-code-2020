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

type Alergen = { Name: string; Aliases: Set<string> }



let rec findAliases knownAliases (alergenNames: Alergen list) =
    alergenNames.Length
    |> function
    | 0 -> knownAliases
    | _ ->
        printfn "%A" alergenNames
        printfn "%A" knownAliases
        let { Name = knownAlergen; Aliases = aliases } =
            alergenNames
            |> List.find (fun { Aliases = aliases } -> aliases.Count = 1)

        let [ knownAlias ] = aliases |> Set.toList
        alergenNames
        |> List.filter (fun { Name = alergen } -> alergen <> knownAlergen)
        |> List.map (fun { Name = alergen; Aliases = aliases } ->
            { Name = alergen
              Aliases = if (aliases |> Set.contains knownAlias) then (aliases |> Set.remove knownAlias) else aliases })
        |> findAliases ((knownAlergen, knownAlias) :: knownAliases)

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
            let alergenAliases =
                lines
                |> List.map (fun (_, ingredients) -> Set.ofList ingredients)
                |> Set.intersectMany

            { Name = alergen
              Aliases = alergenAliases })

    // alergenNames
    findAliases [] alergenNames
    |> List.sortBy (fun (first, _) -> first)
    |> List.map (fun (_, second)-> second)
    |> String.concat ","


solve multilineText
open System.IO
let recepies = File.ReadAllText("input.txt")
solve recepies
