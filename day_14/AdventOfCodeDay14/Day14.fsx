type Instruction =
    { Address: System.UInt64
      Number: System.UInt64 }

let parseInstruction (instructionText: string) =
    printfn "%s" instructionText

    let addressText :: _ :: number :: _ =
        (instructionText.Split ' ') |> Seq.toList

    let _ :: addressEnd :: _ = (addressText.Split '[') |> Seq.toList

    { Number = System.UInt64.Parse(number)
      Address = System.UInt64.Parse(addressEnd |> String.filter ((<>) ']')) }

parseInstruction "mem[8] = 11"


let textInput = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0"

type Mask =
    | Zero of int
    | One of int

let parseMask (maskText: string) =
    // let _ :: _ :: mask :: _ = (maskText.Split ' ') |> Seq.toList

    maskText
    |> Seq.toList
    |> List.indexed
    |> List.filter (fun (_, filter) -> filter <> 'X')

    |> List.map (fun (index, filter) ->
        match filter with
        | '1' -> One(35 - index)
        | '0' -> Zero(35 - index))

parseMask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"

let parseInput (textInput: string) =
    let maskLine :: instructions = (textInput.Split '\n') |> Seq.toList

    (maskLine |> parseMask,
     instructions
     |> Seq.filter ((<>) "")
     |> Seq.map parseInstruction
     |> Seq.toList)

parseInput textInput

let applyMask (number: uint64) =
    function
    | One position -> number ||| ((uint64 1) <<< position)
    | Zero position -> number &&& (~~~((uint64 1) <<< position))

applyMask (uint64 11) (Zero 1)


applyMask (uint64 11) (One 6)
let maskNumber masks (number: uint64) = masks |> List.fold applyMask number

(1 <<< 3) ||| 11
maskNumber [ One 6; Zero 1 ] (uint64 11)



let solve textInput =
    let masks, instructions = parseInput textInput

    instructions
    |> Seq.map (fun { Address = address; Number = number } ->
        { Address = address
          Number = maskNumber masks number })
    |> Seq.toList
    |> List.groupBy (fun { Address = address } -> address)
    |> List.map (fun (_, instructions) -> instructions |> List.last)
    |> List.sumBy (fun { Number = number } -> number)

solve textInput

open System.IO

let programText = File.ReadAllText("input.txt")

// solve programText

let longerInput = "mask = 0X10110X1001000X10X00X01000X01X01101
mem[49559] = 97
mem[18692] = 494387917
mem[9337] = 615452
mem[24695] = 3435491
mem[54748] = 10952137
mem[26017] = 32712432
mask = 00XX1111100100XX1000X1X00001111X1010
mem[16422] = 941878948
mem[15811] = 1427982
mem[55941] = 1648468
mem[50262] = 34782"


let applyMasksToInstuctions (masks, instructions) =
    instructions
    |> Seq.map (fun { Address = address; Number = number } ->
        { Address = address
          Number = maskNumber masks number })
    |> Seq.toList

open System

programText.Split([| "mask = " |], StringSplitOptions.None)
|> Seq.toList
|> List.map (parseInput)
|> List.tail
|> List.collect applyMasksToInstuctions
|> List.groupBy (fun { Address = address } -> address)
|> List.map (fun (_, instructions) -> instructions |> List.last)
|> List.sumBy (fun { Number = number } -> number)

parseInput "0X10110X1001000X10X00X01000X01X01101
mem[49559] = 97"

maskNumber [Zero 35; One 33; Zero 32; One 31; One 30; Zero 29; One 27; Zero 26; Zero 25; One 24; Zero 23; Zero 22; Zero 21; One 19; Zero 18; Zero 16;Zero 15; Zero 13; One 12; Zero 11; Zero 10; Zero 9; Zero 7; One 6; Zero 4; One 3; One 2; Zero 1; One 0] (uint64 97) 

