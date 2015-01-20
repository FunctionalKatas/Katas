 	let t1 = "49927398716"
    let t2 = "49927398717"
    let t3 = "1234567812345678"
    let t4 = "1234567812345670"
    
    let convertToList (number: string) =
        seq { for i in 0 .. (number.Length-1) do yield int (string(number.Chars i))}
        |> List.ofSeq
        |> List.rev
    
    let rec splitList = function
    | [] -> [], []
    | [x]-> [x], []
    | x1::x2::xs -> let xs1, xs2 = splitList xs
                    x1::xs1, x2::xs2
    
    /// Multiply each digit by two and sum the digits if the answer is 
    /// greater than nine to form partial sums for the even digits
    let s2calc s2l =
        let x = s2l * 2        
        if (x > 9) then (int x/10) + x%10
        else x

    let LuhnTest number =
        let cc = convertToList number
        let s1l, s2l = splitList cc        
        let s1 = List.sum s1l
        let s2= s2l 
                |> List.map(fun x-> s2calc x)
                |> List.sum
        (s1 + s2) % 10 = 0
        

    [t1;t2;t3;t4]
    |> List.map LuhnTest