module Change

let (|Negative|_|) x =
    if x < 0 then Some Negative
    else None    

type State = { Denominations: int list; Amount: int }

let UsdCoins = [100; 50; 25; 10; 5; 1]
let EuroCoins = [100; 50; 20; 10; 5; 2; 1]

let WaysOfMakingChange denominations amount =
    let rec WaysOfMakingChange (state: State) =
        match state with
        | { Amount = Negative } -> 0
        | { Amount = 0 } -> 1
        | { Denominations = [] } -> 0
        | { Denominations = first::rest } -> 
            WaysOfMakingChange { state with Denominations = rest } 
            + 
            WaysOfMakingChange { state with Amount = state.Amount - first }

    WaysOfMakingChange { Denominations = denominations; Amount = amount }
