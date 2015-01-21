open System
open Luhn

let Luhn: string->bool =
    let oneTwoCycle = Seq.cycle (seq[1;2])
    let sumDigits number = if number > 9 then number%9 else number
    let isMultipleOf10 x = x % 10 = 0

    Seq.map (string >> int) 
    >> Seq.rev
    >> Seq.map2 (*) oneTwoCycle 
    >> Seq.map sumDigits
    >> Seq.sum
    >> isMultipleOf10

[<EntryPoint>]
let main argv = 
   printfn "49927398716 Should be true, Is (%A)" (Luhn "49927398716")
   printfn "49927398717 Should be false, Is (%A)" (Luhn "49927398717")
   printfn "1234567812345678 Should be false, Is (%A)" (Luhn "1234567812345678")
   printfn "1234567812345670 Should be true, Is (%A)" (Luhn "1234567812345670")
   0 // return an integer exit code

 
