module StringCalculator

open System
open System.Text.RegularExpressions

let Add (arg:string) =
    let Matches pattern s =
        Regex(pattern).Matches(s)
        |> Seq.cast<Match>
        |> Seq.map (fun m -> m.Value)
        
    let ParseDelimiters (s:string) =
        let parsed = 
            Matches "(?<=\[)[^]]+(?=])" s
            |> Array.ofSeq

        match parsed with
        | [||] -> [|s|]
        | _ -> parsed
        
    let SplitCustomDelimiter (s:string) =
        let pos = s.IndexOf("\n")
        s.Substring(0, pos) |> ParseDelimiters, s.Substring(pos)
        
    let (|CustomDelimiter|) (s:string) =
        if s.StartsWith("//") then
            s.Substring(2) |> SplitCustomDelimiter
        else
            [| ","; "\n" |], s

    let parse s =
        match s with
        | "" -> raise (ArgumentException("Missing number"))
        | _ ->
            match Int32.Parse(s) with
            | x when x > 1000 -> 0
            | x -> x

    let args =
        match arg with
        | "" -> []
        | CustomDelimiter (delimiter,text) -> 
            text.Split(delimiter, StringSplitOptions.None)
            |> Seq.map parse
            |> List.ofSeq

    let rec AddRec sum list =
        match list with
        | [] -> sum
        | head::tail -> AddRec (sum + head) tail // tail recursive

    let negative x = x < 0

    let join separator (xs:int seq) =
        String.Join(separator, xs)

    match args with
        | _ when Seq.exists negative args -> 
            let negatives = args |> Seq.where negative |> join ","
            raise (ArgumentException(sprintf "negatives not allowed: %s" negatives))
        | _ ->
            AddRec 0 args

