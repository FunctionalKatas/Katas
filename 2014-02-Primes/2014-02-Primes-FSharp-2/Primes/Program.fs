// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

    
let rec Sieve maxList =
    match maxList with
    | head::tail -> head :: (Sieve <| List.filter (fun x -> x % head <> 0) tail)
    | [] -> []

let Constrained maxNumber = 
     Sieve [2..maxNumber]

let UPPER_LIMIT = 100000

let IsBiggerThan x =    
    if x <= UPPER_LIMIT then        
        true    
    else 
        printfn "First prime number bigger than %i %i" UPPER_LIMIT x
        false


[<EntryPoint>]
let main argv = 
    
    Constrained (UPPER_LIMIT+25000)
    |> Seq.skipWhile (fun x -> IsBiggerThan x)
    |> Seq.skip 43
    |> Seq.take 5
    |> Seq.iteri (fun index value -> printfn "%d %A" index value)
    System.Console.ReadKey true |> ignore
    0 // return an integer exit code


