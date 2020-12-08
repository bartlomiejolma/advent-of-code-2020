let ruleText =
    "light red bags contain 1 bright white bag, 2 muted yellow bags."

open System

[ "muted"; "yellow"; "bags" ] |> String.concat " "

let getOnlyNames (listOfBagInfo: string list) = listOfBagInfo.[1..] |> String.concat " "

type Rule = { Bag: string; CanBeIn: string list }

let singularize (text: string) =
    text.[text.Length - 1] = 's'
    |> function
    | true -> text.[..(text.Length - 2)]
    | false -> text

let asRule canBeIn bag =
    { Bag = singularize bag
      CanBeIn = [ singularize canBeIn ] }



let processSplittedRule (singleRule: string list) =
    let outerBag = singleRule.[0]
    (singleRule.[1].Trim '.').Split ','
    |> Seq.map (fun x ->
        ((x.Trim ' ').Split ' ')
        |> Seq.toList
        |> getOnlyNames
        |> asRule outerBag)
    |> Seq.toList

let parseSingleRule (ruleText: string) =
    ruleText.Split([| " contain" |], StringSplitOptions.None)
    |> Seq.toList
    |> processSplittedRule

parseSingleRule ruleText

let multipleRules = "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags."

let mergeCanBeIn (key, (rules: Rule list)) =
    key, rules |> List.collect (fun x -> x.CanBeIn)

let getAllCanBeIn (multipleRules: string) =
    multipleRules.Split '\n'
    |> Seq.collect parseSingleRule
    |> Seq.toList
    |> List.groupBy (fun { Bag = bag } -> bag)
    |> List.map mergeCanBeIn

getAllCanBeIn multipleRules



let rec countInHowManyCanBe (bag: string) (canBeInRules: (string * string list) list) =
    canBeInRules
    |> List.filter (fun (innerBag, _) -> innerBag = bag)
    |> function
    | [] -> Set([ bag ])
    | [ (_, canBeInList) ] ->
        canBeInList
        |> List.map (fun innerBag -> countInHowManyCanBe innerBag canBeInRules)
        |> Set.unionMany
        |> Set.add bag



multipleRules
|> getAllCanBeIn
|> countInHowManyCanBe "shiny gold bag"
|> Set.count
|> (+) -1


open System.IO

let rulesText = File.ReadAllText("input.txt")

rulesText
|> getAllCanBeIn
|> countInHowManyCanBe "shiny gold bag"
|> Set.count
|> (+) -1
