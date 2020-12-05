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

type Rule = { Code: string; Rule: string -> Option<bool> }

let validateRule text { Code = code; Rule = rule } = rule text

let parseCode codes (head :: text) =

    // rule text
    let rule =
        List.tryFind (fun { Code = code; Rule = rule } -> code = head) codes

    rule 
    |> Option.bind (validateRule text.[0])
    |> function 
       | Some true -> Some head
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


let optionalString value =
    value
    |> System.Int32.TryParse
    |> function
    | (true, value) -> Some value
    | (_) -> None

let inRange min max value = value >= min && value <= max


let checkLength expectedLen text = 
    let textIfTrue =
        function
        | true -> Some text
        | _ -> None
    text |> String.length |> (=) expectedLen |> textIfTrue

let yearRule min max (text: string)  = 

    text
    |> checkLength 4
    |> Option.map int
    |> Option.map (inRange min max)

let byrRule = yearRule 1920 2002
let iyrRule = yearRule 2010 2020
let eyrRule = yearRule 2020 2030
let cidRule text = Some true

let hgt = "190cm"
let len = String.length hgt - 2
hgt.[len..]


let validateRange min max text = 
    text 
    |> System.Int32.TryParse
    |> function
        | (true, number) -> Some (inRange min max number)
        | _ -> None

let hgtRule (text: string) = 
    let len = String.length hgt - 2
    hgt.[len..]
    |> function
        | "cm" -> validateRange 150 193 hgt.[..len - 1]
        | "in" -> validateRange 59 76 hgt.[..len - 1]
        | _ -> None

hgtRule "74in"

let eclRule = function
    | "amb" -> Some true
    | "blu" -> Some true
    | "brn" -> Some true
    | "gry" -> Some true
    | "grn" -> Some true
    | "hzl" -> Some true
    | "oth" -> Some true
    | _ -> None

eclRule "amb"
   
"2000" |> byrRule

hgtRule "190"

System.Int32.TryParse "000000001"


let pidRule (text:string) = 
    text
    |> checkLength 9
    |> Option.map System.Int32.TryParse
    |> function
        | Some (true, number) -> Some true
        | _ -> None

pidRule "000000001"

"#123".Chars 0

"#123".[1..]
let checkStartsWith (text:string)=
    text.Chars 0
    |> (=) '#'
    |> function
        | true -> Some text.[1..] 
        | _ -> None

let containsOnlyAllowed allowed text = 
    text
    |>  Seq.toList |> List.forall (fun x -> List.contains x allowed)
 
let hclRule (text: string) = 
    let allowed = ['a'..'h'] @ ([0..9] |> List.map (string >> char))

    let containsHexes = containsOnlyAllowed allowed
    
    text 
    |> checkStartsWith
    |> Option.bind (checkLength 6)
    |> Option.map containsHexes
    |> function
        | Some true -> Some true
        | _ -> None 

hclRule "#123abc"
hclRule "#123abz"
hclRule "123abc"

byrRule "2002"

pidRule "0123456789"

System.Globalization.NumberStyles.HexNumber
System.Int64.TryParse("123abc")

let allowed = ['a'..'h'] @ ([0..9] |> List.map (string >> char))

"123abc"


Some "200" |> Option.bind optionalString

// let codes = [ { Code = "byr"; Rule = byrRule } ]

// codes
//     |> List.map (fun x -> x.Code) 
//     |> List.filter (fun x -> x <> "cid") 

let validatePassport passportString =
    let codes = [ 
        { Code = "byr"; Rule = byrRule }
        { Code = "iyr"; Rule = iyrRule }
        { Code = "eyr"; Rule = eyrRule }
        { Code = "cid"; Rule = cidRule }
        { Code = "hgt"; Rule = hgtRule }
        { Code = "ecl"; Rule = eclRule }
        { Code = "pid"; Rule = pidRule }
        { Code = "hcl"; Rule = hclRule }
     ]

    let requiredCodes = 
        codes
        |> List.map (fun x -> x.Code) 
        |> List.filter (fun x -> x <> "cid") 

    let passport = parseSinglePassport codes passportString
    List.forall (fun code -> List.contains code passport) requiredCodes


//failing
validatePassport "eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926"

validatePassport "iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946"

validatePassport "hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277"

validatePassport "hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007"

//passsing
validatePassport "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f"

validatePassport "eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm"

validatePassport "hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022"

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


"0b00000101" |> int