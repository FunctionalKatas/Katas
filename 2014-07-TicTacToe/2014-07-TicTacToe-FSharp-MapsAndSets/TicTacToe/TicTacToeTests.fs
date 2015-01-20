module TicTacToeTests

open TicTacToe.Core
open NUnit.Framework

type TicTacToeTests() =

    [<Test>]
    member this.Win() =
        let pos = 
            NewBoard
            |> Move X TopLeft
            |> Move X Center
            |> Move X BottomRight
        Assert.True(IsWin X pos)

    [<Test>]
    member this.Available() =
        let pos = 
            NewBoard
            |> Move X TopLeft
            |> Move O TopMiddle
            |> Move X TopRight
            |> Move O MiddleLeft
            |> Move X MiddleRight
            |> Move O BottomLeft
            |> Move X BottomMiddle
            |> Move O BottomRight
        Assert.AreEqual(Set[Center], Available pos)

    [<Test>]
    member this.BestMove() =
        let pos = 
            NewBoard
            |> Move X TopLeft
            |> Move O TopMiddle
            |> Move X Center
            |> Move O TopRight
        Assert.AreEqual(BottomRight, BestMove X pos)
