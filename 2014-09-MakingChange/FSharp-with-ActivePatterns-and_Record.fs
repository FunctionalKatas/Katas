module Change

type State = { Denominations: int list; Amount: int }

let (|NegativeAmount|ZeroAmount|ZeroCoins|Remaining|) (state: State) =
    if state.Amount < 0 then NegativeAmount
    elif state.Amount = 0 then ZeroAmount
    elif state.Denominations = [] then ZeroCoins
    else Remaining (state.Denominations.Head, state.Denominations.Tail, state.Amount)    

let UsdCoins = [100; 50; 25; 10; 5; 1]
let EuroCoins = [100; 50; 20; 10; 5; 2; 1]

let WaysOfMakingChange denominations amount =
    let rec WaysOfMakingChange (state: State) =
        match state with
        | ZeroAmount -> 1
        | ZeroCoins -> 0
        | NegativeAmount -> 0
        | Remaining (firstCoin, restOfCoins, amount) -> 
            WaysOfMakingChange { state with Denominations = restOfCoins } 
            + 
            WaysOfMakingChange { state with Amount = amount - firstCoin }

    WaysOfMakingChange { Denominations = denominations; Amount = amount }


// Using Partial Application we can create change counters for any denominstaions
let UsdCounter = WaysOfMakingChange UsdCoins
let EuroCounter = WaysOfMakingChange EuroCoins
let adHocCounter = WaysOfMakingChange [2;4;8]



