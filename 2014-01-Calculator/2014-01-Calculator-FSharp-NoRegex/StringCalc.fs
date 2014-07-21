module StringCalc

    open System

    exception NegativesFound of int[]
    let DefaultDelimiters = [|",";"\n"|]

    let PreventNegatives nums =
        match nums |> Array.filter (fun n -> n < 0) with
        | negatives when negatives.Length > 0 -> raise (NegativesFound negatives)
        | _ -> nums
        
    let ParseDelimiters (expr: string) =
        let parts = expr.Split([|'\n'|], 2)
        let customDelimiters = parts.[0].Split([|"/";"[";"]"|], StringSplitOptions.RemoveEmptyEntries)
        (Array.append DefaultDelimiters customDelimiters, parts.[1])

    let Parse (expr: string) =
        match expr with
        | _ when expr.StartsWith "//" -> expr |> ParseDelimiters 
        | _ -> (DefaultDelimiters, expr)

    let Split (expr: string[] * string) =
        let delimiters, numbers = expr
        numbers.Split (delimiters, StringSplitOptions.RemoveEmptyEntries)
        |> Array.map Int32.Parse

    let Filter = PreventNegatives >> Array.filter (fun n -> n <= 1000)
    
    let Sum = Array.sum

    let Add = Parse >> Split >> Filter >> Sum