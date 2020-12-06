let text = "a
b
c"




let parseSetOfAnswers (text:string) = 
    text.Split '\n'
    |> Seq.collect Seq.toList
    |> Seq.distinct
    |> Seq.length


parseSetOfAnswers "a
a
a"

parseSetOfAnswers "abc
abd"

let textInput = "a
a
a

abc
abd"

open System

let parseAllGroups (textInput:string) = 
    textInput.Split([| "\n\n" |], StringSplitOptions.None)
    |> Seq.sumBy parseSetOfAnswers

parseAllGroups textInput

open System.IO

let groupsText = File.ReadAllText("input.txt")

parseAllGroups groupsText