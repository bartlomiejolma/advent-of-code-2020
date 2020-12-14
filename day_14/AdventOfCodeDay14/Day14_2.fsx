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

type StandardMask =
    | Zero of int
    | One of int

type Mask =
    | StandardMask of StandardMask
    | Float of int

let parseMask (maskText: string) =
    // let _ :: _ :: mask :: _ = (maskText.Split ' ') |> Seq.toList

    maskText
    |> Seq.toList
    |> List.indexed

    |> List.map (fun (index, filter) ->
        match filter with
        | '1' -> StandardMask(One(35 - index))
        | '0' -> StandardMask(Zero(35 - index))
        | 'X' -> Float(35 - index))

parseMask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"

let parseInput (textInput: string) =
    let maskLine :: instructions = (textInput.Split '\n') |> Seq.toList

    (maskLine |> parseMask,
     instructions
     |> Seq.filter ((<>) "")
     |> Seq.map parseInstruction
     |> Seq.toList)

parseInput textInput

let applyStandardMask (number: uint64) =
    function
    | One position -> number ||| ((uint64 1) <<< position)
    | Zero _ -> number

applyStandardMask (uint64 11) (Zero 1)


applyStandardMask (uint64 11) (One 6)

let setOne number position = number ||| ((uint64 1) <<< position)

let setZero number position =
    number &&& (~~~((uint64 1) <<< position))

let applyFloatMask (numbers: uint64 list) (Float position) =
    numbers
    |> List.collect (fun number ->
        [ setOne number position
          setZero number position ])

applyFloatMask [ 58UL ] (Float 0)

applyFloatMask [ 58UL; 59UL ] (Float 5)

let maskNumber masks (number: uint64) =
    let standardMaskedAddres =
        masks
        |> List.filter (function
            | StandardMask _ -> true
            | _ -> false)
        |> List.map (fun (StandardMask mask) -> mask)
        |> List.fold applyStandardMask number

    masks
    |> List.filter (function
        | Float _ -> true
        | _ -> false)
    |> List.fold applyFloatMask [ standardMaskedAddres ]

(1 <<< 3) ||| 11

maskNumber
    [ StandardMask(One 6)
      StandardMask(Zero 1) ]
    (uint64 11)


maskNumber
    [ StandardMask(One 1)
      StandardMask(One 4)
      StandardMask(Zero 2)
      StandardMask(Zero 3) ]
    (uint64 42)

maskNumber
    [ StandardMask(One 1)
      StandardMask(One 4)
      StandardMask(Zero 2)
      StandardMask(Zero 3)
      Float 5
      Float 0 ]
    (uint64 42)



open System.IO

let programText = File.ReadAllText("input.txt")

let longerInput = "mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1"

parseMask "000000000000000000000000000000X1001X"


let applyMasksToInstuctions (masks, instructions) =
    instructions
    |> Seq.collect (fun { Address = address; Number = number } ->
        maskNumber masks address
        |> List.map (fun maskedAddress ->
            { Address = maskedAddress
              Number = number }))
    |> Seq.toList

open System



let solve (input: string) =
    input.Split([| "mask = " |], StringSplitOptions.None)
    |> Seq.toList
    |> List.map (parseInput)
    |> List.tail
    |> List.collect applyMasksToInstuctions
    |> List.groupBy (fun { Address = address } -> address)
    |> List.map (fun (_, instructions) -> instructions |> List.last)
    |> List.sumBy (fun { Number = number } -> number)

solve longerInput

solve programText

parseInput "0X10110X1001000X10X00X01000X01X01101
mem[49559] = 97"
