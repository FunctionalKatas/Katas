
type Player = | X | O

let opponent player = match player with | X -> O | O -> X

type Position = Player option

exception BoardError
exception MoveError

type Board = List<Position>

let move player pos board =
    if List.nth board pos |> Option.isSome then raise MoveError

    let start = board |> Seq.take pos |> List.ofSeq
    let finish = board |> Seq.skip (pos + 1) |> List.ofSeq
    start @ (Some player) :: finish

let lines board =
    match board with
    | [ a1; a2; a3; b1; b2; b3; c1; c2; c3] ->
        [ (a1, a2, a3); (b1, b2, b3); (c1, c2, c3); // rows
          (a1, b1, c1); (a2, b2, c2); (a3, b3, c3); // columns
          (a1, b2, c3);  (a3, b2, c1); // diagonals
        ]
    | _ -> raise BoardError

let (|Win|Draw|InProgress|) board =
    let boardLines = lines board
    if List.exists ((=) (Some X, Some X, Some X)) boardLines then Win X
    else if List.exists ((=) (Some O, Some O, Some O)) boardLines then Win O
    else if List.exists ((=) None) board then InProgress
    else Draw

let availableMoves board =
    board
    |> List.mapi  (fun i position -> i, position)
    |> List.filter (snd >> Option.isNone)
    |> List.map fst

let rec score player board =
    match board with
    | Win player -> availableMoves board |> List.length |> (+) 1
    | Draw -> 0
    | _ ->
        let opp = opponent player
        board
        |> move opp (bestMove opp board)
        |> score opp
        |> (~-)

and bestMove player board =
    availableMoves board
    |> List.maxBy (fun pos -> move player pos board |> score player)

let (emptyBoard: Board) = List.init 9 (fun _ -> None)

// Display functions

let printLine (a, b, c) =
    let displayPosition position =
        match position with
        | Some X -> "X"
        | Some O -> "O"
        | None -> "-"
    printfn "%s %s %s" (displayPosition a) (displayPosition b) (displayPosition c)

let printBoard (board: Board) =
    let tripelate board =
        match board with
        | [ a; b; c; d; e; f; g; h; i] -> [(a, b, c); (d, e, f); (g, h, i)]
        | _ -> raise BoardError
    tripelate board |> List.iter printLine
    board

let evaluate board =
    match board with
        | Win X -> printfn "X wins"
        | Win O -> printfn "O wins"
        | InProgress -> printfn "In Progress"
        | Draw -> printfn "Draw"
    board

// Example Games

emptyBoard
|> move X 0
|> move O 3
|> move X 4
|> move O 5
|> evaluate
|> printBoard
|> availableMoves
|> printfn "Available moves: %A"
;; // In Progress

emptyBoard
|> move X 0
|> move O 3
|> move X 4
|> move O 5
|> move X 8
|> evaluate
|> printBoard
;; // X wins

emptyBoard
|> move X 3
|> move O 4
|> move X 0
|> move O 6
|> move X 1
|> move O 2
|> evaluate
|> printBoard
;; // O wins

emptyBoard
|> move X 0
|> move O 3
|> move X 4
|> move O 5
|> evaluate
|> printBoard
|> bestMove X
|> printfn "X best move: %d"
;;
