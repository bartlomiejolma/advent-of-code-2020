let ruleText =
    "light red bags contain 1 bright white bag, 2 muted yellow bags."

open System

[ "muted"; "yellow"; "bags" ] |> String.concat " "


let singularize (text: string) =
    text.[text.Length - 1] = 's'
    |> function
    | true -> text.[..(text.Length - 2)]
    | false -> text

let getOnlyNames (head :: tail) =
    System.Int32.TryParse(head)
    |> function
    | (true, value) -> (int (head), tail |> String.concat " " |> singularize)
    | _ -> (0, "")

type Rule =
    { Bag: string
      Contains: (int * string) list }


let asRule bag contains =
    { Bag = singularize bag
      Contains = contains |> Seq.toList }

asRule "foo"

let processSplittedRule (singleRule: string list) =
    let outerBag = singleRule.[0]
    (singleRule.[1].Trim '.').Split ','
    |> Seq.map (fun x ->
        ((x.Trim ' ').Split ' ')
        |> Seq.toList
        |> getOnlyNames)
    |> asRule outerBag


let parseSingleRule (ruleText: string) =
    ruleText.Split([| " contain" |], StringSplitOptions.None)
    |> Seq.toList
    |> processSplittedRule

parseSingleRule ruleText

parseSingleRule "faded blue bags contain no other bags."

let multipleRules = "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags."


let getAllCanBeIn (multipleRules: string) =
    multipleRules.Split '\n'
    |> Seq.map parseSingleRule
    |> Seq.toList


getAllCanBeIn multipleRules



let rec countHowManyContains (bag: string) (containRules) =
    containRules
    |> List.filter (fun { Bag = innerBag } -> innerBag = bag)
    |> function
    | [] -> 0
    | [ ({ Contains = contains }) ] ->
        contains
        |> List.sumBy (fun (count, innerBag) -> count * (countHowManyContains innerBag containRules))
        |> (+) 1



let totalContain multipleRules = 
    multipleRules
    |> getAllCanBeIn
    |> countHowManyContains "shiny gold bag"
    |> (+) -1

let otherRules = "shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags." 

totalContain otherRules

open System.IO

let rulesText = File.ReadAllText("input.txt")

totalContain rulesText

