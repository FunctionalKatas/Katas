(*
Making Change
How many different ways can we make change of €1, given coins with the denominations 50c, 20c, 10c, 5c, 2c and 1c?
 Write a general procedure to compute the number of ways to change any given amount of money using any given set of denominations.
*)

let onesCount change =
    if(change > 1) then
        change % 1
    else
        0
let calculateChange change =
    let onescount = onesCount change
    let result = { OnesFull = onesCount}
    result

let inline mult (denomination:int)(multiplier:float) =
    (float denomination) * multiplier

let calculateTotal denominations =    
    denominations.OnesFull +  mult denominations.Fifties * 0.50 + mult denominations.Twenties  0.20 + mult denominations.Tens  0.10 + denominations.Fives * 0.05 + denominations.Twos *0.02 +denominations.OneCent


module Testing =

    let changeGivenIsTheChange (change) =
        let result = calculateChange change 
                     |> calculateTotal 
        result = change
