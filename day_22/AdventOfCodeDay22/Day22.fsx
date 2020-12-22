open System



let textInput = "Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10"

let parsePlayerText (playerText: string) =
    let _ :: cards =
        playerText.Trim().Split '\n' |> Seq.toList

    cards |> List.map int


let rec play player1Deck player2Deck =
    player1Deck
    |> function
    | [] -> player2Deck
    | player1Top :: player1Rest ->
        player2Deck
        |> function
        | [] -> player1Deck
        | player2Top :: player2Rest ->
            player1Top > player2Top
            |> function
            | true -> play (player1Rest @ [ player1Top; player2Top ]) player2Rest
            | false -> play player1Rest (player2Rest @ [ player2Top; player1Top ])

let solve (textInput: string) =
    let player1Deck :: player2Deck :: _ =
        textInput.Split([| "Player " |], StringSplitOptions.RemoveEmptyEntries)
        |> Seq.toList
        |> List.map parsePlayerText

    play player1Deck player2Deck
    |> List.rev
    |> List.indexed
    |> List.sumBy (fun (index, card) -> (index + 1) * card)

solve textInput


open System.IO

let puzzle = File.ReadAllText("input.txt")
solve puzzle
