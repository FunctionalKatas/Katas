module StringCalc

    open System

    exception NegativesFound of int[]
    let DefaultDelimiters = [|",";"\n"|]

    let PreventNegatives nums =
        match nums |> Array.filter (fun n -> n < 0) with
        | negatives when negatives.Length > 0 -> raise (NegativesFound negatives)
        | _ -> nums

    let SumNumbers (expr: string[] * string) =
        let (delimiters, numbers) = expr
        numbers.Split (delimiters, StringSplitOptions.RemoveEmptyEntries)
        |> Array.map Int32.Parse
        |> PreventNegatives
        |> Array.filter (fun n -> n <= 1000)
        |> Array.sum

    let ParseDelimiters (expr: string) =
        let parts = expr.Split([|'\n'|], 2)
        let delimiters = parts.[0].Split([|"/";"[";"]"|], StringSplitOptions.RemoveEmptyEntries)
        (delimiters, parts.[1])
        
    let Add expr =
        match expr with
        | "" -> 0
        | _ when expr.StartsWith "//" -> expr |> ParseDelimiters |> SumNumbers
        | _ -> (DefaultDelimiters, expr) |> SumNumbers