open System
open Luhn

let sumDigits n = Math.DivRem(n, 10) ||> (+)
let isMultipleOf x y = y % x = 0

let Luhn: string->bool =
    Seq.map (string >> int) 
    >> Seq.rev
    >> Seq.map2 (*) (Seq.cycle (seq[1;2])) 
    >> Seq.map sumDigits
    >> Seq.sum
    >> isMultipleOf 10

[<EntryPoint>]
let main argv = 
   printfn "49927398716 Should be true, Is (%A)" (Luhn "49927398716")
   printfn "49927398717 Should be false, Is (%A)" (Luhn "49927398717")
   printfn "1234567812345678 Should be false, Is (%A)" (Luhn "1234567812345678")
   printfn "1234567812345670 Should be true, Is (%A)" (Luhn "1234567812345670")
   0 // return an integer exit code

 
