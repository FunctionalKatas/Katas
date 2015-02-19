open System
let l = "love "
let repeat n s = String.replicate n s

let triangle (loves:int, decrement:int) =     
    let rec ptr  loves decrement =        
        match loves <= 0 with 
        | true ->()
        | false -> repeat decrement " " + repeat loves l 
                   |>Console.WriteLine
                   ptr  (loves-1) (decrement+2 )    
    ptr loves decrement

let boob i =    
    let halfLoveCount = i / 2
    let rec bo (loves:int) i  =
        match loves > halfLoveCount with 
        | true -> ()
        | false -> repeat i " " + repeat loves l  + repeat (i*3) " " + repeat loves l 
                   |>Console.WriteLine
                   bo (loves+1) (i-2)
    bo 1 (i-2) 

boob 10
triangle( 10, 1) 