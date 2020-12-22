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

type Result = { Winner: int; Deck: int list }

let rec play player1Deck player2Deck gamesPlayed: Result =

    gamesPlayed
    |> Set.contains (player1Deck, player2Deck)
    |> function
    | true ->
        { Winner = 1
          Deck = player1Deck @ player2Deck }
    | false ->
        player1Deck
        |> function
        | [] -> { Winner = 2; Deck = player2Deck }
        | player1Top :: player1Rest ->
            player2Deck
            |> function
            | [] -> { Winner = 1; Deck = player1Deck }
            | player2Top :: player2Rest ->

                (player1Top
                 <= player1Rest.Length
                 && player2Top <= player2Rest.Length)
                |> function
                | true ->
                    let { Winner = subgameWinner } =
                        play player1Rest.[0..player1Top - 1] player2Rest.[0..player2Top - 1] Set.empty

                    subgameWinner = 1
                | false -> player1Top > player2Top
                |> function
                | true ->
                    play
                        (player1Rest @ [ player1Top; player2Top ])
                        player2Rest
                        (gamesPlayed.Add(player1Deck, player2Deck))
                | false ->
                    play
                        player1Rest
                        (player2Rest @ [ player2Top; player1Top ])
                        (gamesPlayed.Add(player1Deck, player2Deck))

let solve (textInput: string) =
    let player1Deck :: player2Deck :: _ =
        textInput.Split([| "Player " |], StringSplitOptions.RemoveEmptyEntries)
        |> Seq.toList
        |> List.map parsePlayerText

    let { Deck = winnerDeck } = play player1Deck player2Deck Set.empty

    winnerDeck
    |> List.rev
    |> List.indexed
    |> List.sumBy (fun (index, card) -> (index + 1) * card)

solve textInput


solve "Player 1:
43
19

Player 2:
2
29
14"

open System.IO

let puzzle = File.ReadAllText("input.txt")
solve puzzle
