(*  Simple solution *)

let rec countingChange amount denomination =
    if(amount = 0) then 1
    elif(amount < 0 || denomination = []) then 0
    else
        let head, tail = denomination.Head, denomination.Tail
        (countingChange amount tail)+ countingChange (amount-head) denomination

let countingUs amount =
    countingChange amount [50;25;10;5;1]


countingUs 100
