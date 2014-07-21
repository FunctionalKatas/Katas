namespace TicTacToe

module Core =

    type Position = TopLeft | TopMiddle | TopRight 
                    | MiddleLeft | Center | MiddleRight 
                    | BottomLeft | BottomMiddle| BottomRight

    type Player = X | O

    type Board = Map<Position, Player Option>

    let NewBoard: Board = 
            Map [ (TopLeft, None); (TopMiddle, None); (TopRight, None); 
                  (MiddleLeft, None); (Center, None); (MiddleRight, None); 
                  (BottomLeft, None); (BottomMiddle, None); (BottomRight, None) ]


    let wins = set [ set [ TopLeft; TopMiddle; TopRight ] ;
                     set [ MiddleLeft; Center; MiddleRight ] ;
                     set [ BottomLeft; BottomMiddle; BottomRight ] ;
                     set [ TopLeft; MiddleLeft; BottomLeft ] ;
                     set [ TopMiddle; Center;  BottomMiddle ] ;
                     set [ TopRight; MiddleRight; BottomRight ] ;
                     set [ TopLeft; Center; BottomRight ] ;
                     set [ TopRight; Center; BottomLeft ] ; ]

    let FindPositions (player: Player Option) (board: Board) =
        board
        |> Map.filter (fun _ mark -> mark = player) 
        |> Map.toSeq
        |> Seq.map fst
        |> Set.ofSeq

    let Available = FindPositions None

    let IsWin (player: Player) (board: Board) =
        let playersSquares = 
            board |> FindPositions (Some player)
        wins 
        |> Set.exists (fun win -> win.IsSubsetOf playersSquares)

    let IsDraw = FindPositions None >> Set.isEmpty

    let Opponent = function
        | X -> O
        | _ -> X

    let rec Score (player: Player) (board: Board) =
        if (IsWin player board) then board |> Available |> Set.count |> (+) 1
        else if (IsDraw board) then 0
        else 
            let opponent = Opponent player
            let opponentsBestMove = BestMove opponent board
            let newBoard = board.Add(opponentsBestMove, Some opponent)
            -Score opponent newBoard

    and BestMove (player: Player) (board: Board): Position =
        Available board
        |> Set.toList
        |> List.maxBy (fun m -> Score player (board.Add(m, Some player))) 

    let Move (player: Player) (position: Position) (board: Board) =
        board.Add(position, Some player)

