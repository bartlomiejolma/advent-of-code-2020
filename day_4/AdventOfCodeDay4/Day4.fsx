let textInput = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in"

// let combinePassport acc substring =
//     match substring with
//     | "" -> [] :: acc
//     | _ -> [substring] :: (List.head acc)
// let getSinglePassport (textInput: string) =
//     textInput.Split '\n'
//     |> Seq.toList
//     |> List.fold combinePassport List.Empty

open System

let splitByNewLine (substring: string) = substring.Split '\n' |> Seq.toList


let parseCode codes codeCandidate =
    let head = List.head codeCandidate
    match (List.contains head codes) with
    | true -> Some head
    | _ -> None

let codes =
    [ "byr"
      "iyr"
      "eyr"
      "hgt"
      "hcl"
      "ecl"
      "pid"
      "cid" ]

List.filter ((fun x -> x <> "cid")) codes

let extractCode (entry: string) =

    entry.Split ':' |> Seq.toList


let parseSinglePassport codes (passportString: string) =

    passportString.Split ' '
    |> Seq.toList
    |> (List.map splitByNewLine >> List.concat)
    |> List.map (extractCode >> parseCode codes)
    |> List.choose id

let validatePassport passportString =
    let codes =
        [ "byr"
          "iyr"
          "eyr"
          "hgt"
          "hcl"
          "ecl"
          "pid"
          "cid" ]

    let requiredCodes = List.filter (fun x -> x <> "cid") codes

    let passport = parseSinglePassport codes passportString
    List.forall (fun code -> List.contains code passport) requiredCodes



let countValidPassport (textInput: string) =
    textInput.Split([| "\n\n" |], StringSplitOptions.None)
    |> Seq.toList
    |> List.map validatePassport
    |> List.filter id
    |> List.length 

countValidPassport textInput


open System.IO // Name spaces can be opened just as modules

let msg = File.ReadAllText("input.txt")

countValidPassport msg

[ '1', '2', '3' ] |> List.collect (fun x -> [ x ])

[ Some 1; None; Some 2 ] |> List.choose id
