namespace MemoryGameSPA

open WebSharper.JavaScript
open WebSharper
open WebSharper.UI
open WebSharper.UI.Html
open WebSharper.UI.Client 

[<JavaScript>]
module MemoryGameSPA =

    type Card = {
        Id: int
        Value: string
        IsFlipped: bool
        IsMatched: bool
    }

    type Player = {
        Name: string
        Score: int
        TimeLeft: int
        Lives: int
    }

    type GameState = {
        Cards: list<Card>
        Flipped: list<int>
        Players: list<Player>
        CurrentPlayer: int
        IsGameOver: bool
        Streak: int  
    }

    let emojis = 
        [ "🐶"; "🐱"; "🦊"; "🐼"; "🐵"; "🐸"; "🐙"; "🦄" ]

    let random = 
        System.Random()

    let playerNamesVar = 
        Var.Create (None : option<list<string>>)

    let createDeck size =
        emojis
        |> List.take size
        |> List.collect (fun emoji -> [emoji; emoji])
        |> List.sortBy (fun _ -> random.Next())
        |> List.mapi (fun i v -> { Id = i; Value = v; IsFlipped = false; IsMatched = false })

    let createPlayers names =
        names |> List.map (fun name -> { Name = name; Score = 0; TimeLeft = 60; Lives = 5 })

    let initGame size names =
        {
            Cards = createDeck size
            Flipped = []
            Players = createPlayers names
            CurrentPlayer = 0
            IsGameOver = false
            Streak = 0
        }

    let gameStateVar = 
        Var.Create (initGame 8 ["Játékos1"; "Játékos2"])

    let allMatched cards = 
        cards |> List.forall (fun c -> c.IsMatched)

    let flipCard state index =
        if List.contains index state.Flipped || state.Cards.[index].IsMatched then state
        else
            let cards = state.Cards |> List.mapi (fun i c ->
                if i = index then { c with IsFlipped = true } else c)
            { state with Cards = cards; Flipped = index :: state.Flipped }
    
    let shuffleUnmatched cards =
        let unmatched = cards |> List.filter (fun c -> not c.IsMatched)
        let shuffled = unmatched |> List.sortBy (fun _ -> random.Next())
        let matched = cards |> List.filter (fun c -> c.IsMatched)

    // Új azonosítók a kártyákhoz
        let all = matched @ shuffled
        all |> List.mapi (fun i c -> { c with Id = i })

    let checkMatch state =
        match state.Flipped with
        | [i1; i2] ->
            let c1 = state.Cards.[i1]
            let c2 = state.Cards.[i2]
            if c1.Value = c2.Value then
                let cards = state.Cards |> List.mapi (fun i c ->
                    if i = i1 || i = i2 then { c with IsMatched = true } else c)
                let players = state.Players |> List.mapi (fun i p ->
                    if i = state.CurrentPlayer then { p with Score = p.Score + 1 } else p)
                let isGameOver = allMatched cards
                let newStreak = state.Streak + 1

                let finalCards =
                    if newStreak >= 3 then shuffleUnmatched cards
                    else cards

                {
                    state with
                        Cards = finalCards
                        Flipped = []
                        Players = players
                        IsGameOver = isGameOver
                        Streak = newStreak % 3  // Újraindítjuk, ha elérte a 3-at
                }
            else
                let cards = state.Cards |> List.mapi (fun i c ->
                    if i = i1 || i = i2 then { c with IsFlipped = false } else c)
                let players = state.Players |> List.mapi (fun i p ->
                    if i = state.CurrentPlayer then { p with Lives = p.Lives - 1 } else p)
                let currentOutOfLives = players.[state.CurrentPlayer].Lives <= 0
                let isGameOver = currentOutOfLives || allMatched cards
                {
                    state with
                        Cards = cards
                        Flipped = []
                        Players = players
                        IsGameOver = isGameOver
                        Streak = 0  // Hibánál lenullázzuk
                }
        | _ -> state

    let nextPlayer state =
        let next = (state.CurrentPlayer + 1) % state.Players.Length
        { state with CurrentPlayer = next; Flipped = [] }

    let tickTimer state =
        if state.IsGameOver then state
        else
            let players = state.Players |> List.mapi (fun i p ->
                if i = state.CurrentPlayer && p.TimeLeft > 0 then
                    { p with TimeLeft = p.TimeLeft - 1 }
                else p)

            let currPlayer = players.[state.CurrentPlayer]
            let allTimeOut = players |> List.forall (fun p -> p.TimeLeft <= 0)

            if allTimeOut then
                { state with Players = players; IsGameOver = true }
            elif currPlayer.TimeLeft <= 0 then
                let next = (state.CurrentPlayer + 1) % players.Length
                { state with Players = players; CurrentPlayer = next }
            else
                { state with Players = players }

    let onCardClick index (e: Dom.MouseEvent) =
        let state = gameStateVar.Value
        if not state.IsGameOver && state.Flipped.Length < 2 then
            let state1 = flipCard state index
            gameStateVar.Value <- state1

            if state1.Flipped.Length = 2 then
                async {
                    do! Async.Sleep 1000 
                    let newState = checkMatch gameStateVar.Value

                    let didMatch =
                        match state1.Flipped with
                        | [i1; i2] -> state1.Cards.[i1].Value = state1.Cards.[i2].Value
                        | _ -> false

                    let finalState =
                        if not didMatch then nextPlayer newState
                        else newState

                    gameStateVar.Value <- finalState
                } |> Async.Start

    let cardView index (card: Card) =
        let cardClass =
            [
                yield "card"
                if card.IsFlipped || card.IsMatched then yield "flipped"
                if card.IsMatched then yield "matched"
            ]
            |> String.concat " "

        div [
            attr.``class`` cardClass
            on.click (fun _ -> onCardClick index)
        ] [
            div [attr.``class`` "card-inner"] [
                div [attr.``class`` "card-front"] [
                    text "❓"
                ]
                div [attr.``class`` "card-back"] [
                    text card.Value
                ]
            ]
        ]

    
    let playerStatsView state =
        div [] (
            state.Players
            |> List.mapi (fun i p ->
                div [
                    attr.style (
                        if i = state.CurrentPlayer then
                            "font-weight:bold; color:green;"
                        else
                            "color:black;"
                    )
                ] [
                    text $"{p.Name} - Pont: {p.Score}, Idő: {p.TimeLeft}s, Életek: {p.Lives}"
                ])
        )

    let cardsView state =
        div [attr.``class`` "cards"] (
            state.Cards
            |> List.mapi (fun i c -> cardView i c)
        )


    let startTimer (e: Dom.MouseEvent) =
        let rec loop () =
            async {
                do! Async.Sleep 1000
                gameStateVar.Value <- tickTimer gameStateVar.Value
                if not gameStateVar.Value.IsGameOver then
                    do! loop ()
            }
        loop () |> Async.Start

    let resetGame (e: Dom.MouseEvent) = 
        startTimer(e)
        gameStateVar.Value <- initGame 8 ["Játékos1"; "Játékos2"]
        

    let nameInputView =
        let name1 = Var.Create ""
        let name2 = Var.Create ""

        div [attr.style "margin: 2rem;"] [
            h3 [] [text "Add meg a játékosok neveit:"]
            div [] [
                label [] [text "Játékos 1: "]
                Doc.InputType.Text [] name1
            ]
            div [attr.style "margin-top: 1rem;"] [
                label [] [text "Játékos 2: "]
                Doc.InputType.Text [] name2
            ]
            button [
                attr.style "margin-top: 1rem;"
                on.click (fun _ ->
                    let names = [name1.Value; name2.Value] |> List.map (fun n -> if n = "" then "Névtelen" else n)
                    playerNamesVar.Value <- Some names
                    gameStateVar.Value <- initGame 8 names
                    startTimer
                )
            ] [text "▶️ Játék indítása"]
        ]

    let mainView =
        Doc.BindView (function
            | Some _ -> // Ha megadták a neveket, indulhat a játék
                div [attr.``class`` "game-board"] [
                    Doc.BindView playerStatsView gameStateVar.View
                    Doc.BindView cardsView gameStateVar.View
                    Doc.BindView (fun state ->
                        if state.IsGameOver then
                            let maxScore = 
                                state.Players |> List.maxBy (fun p -> p.Score) |> fun p -> p.Score
                            let noLives = 
                                state.Players |> List.exists (fun p -> p.Lives <= 0)
                            let winners = 
                                state.Players |> List.filter (fun p -> p.Score = maxScore)
                            let winnerText =
                                if winners.Length = 1 then
                                    $"🏆 A győztes: {winners.Head.Name}!"
                                else
                                    "🤝 Döntetlen! Győztesek: " + (winners |> List.map (fun p -> p.Name) |> String.concat ", ")

                            div [attr.style "margin-top:1rem;"] [
                                div [attr.style "color:red; font-weight:bold; margin-bottom:0.5rem;"] [
                                    text "Véget ért a játék!"
                                ]
                                div [attr.style "margin-bottom:0.5rem; color:blue; font-weight:bold;"] [
                                    text winnerText
                                ]
                                button [
                                    attr.``class`` "restart-button"
                                    on.click (fun _ -> resetGame)
                                ] [text "🔁 Új játék indítása"]
                            ]
                        else Doc.Empty
                    ) gameStateVar.View
                ]
            | None -> nameInputView
        ) playerNamesVar.View
        

module Mainmodule =
    [<SPAEntryPoint>]
    let Main () =
        Doc.RunById "main" MemoryGameSPA.mainView
        MemoryGameSPA.startTimer
        