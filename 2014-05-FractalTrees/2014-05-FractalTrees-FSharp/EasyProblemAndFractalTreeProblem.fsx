module ArrayOfNElements 
//Create an array of N integers, where N is specified as input. This can be any array with N integers. For example, for N=4 you could return [1,1,1,1] or [1,2,3,4]
open System

let f n = 
    [1..n]


let main() =
    let input = Console.ReadLine()
    let n = int input
    printfn "%d" (f n).Length

 
main()

module TreeFractal =
    let maxWidth = 100

    let printEmptyLine width=
        [1..width]
            |> List.iter (fun _ -> printf "_" )
            
    let createLeaf (width:int, len:int) =
         let palitoPosition = width/2 
         let halfLenght = len /2
         palitoPosition, halfLenght,len

    let rec printLeftLeaf posStart len posOrig=        
        if (posStart > 1 && len > 1) then
            let leng = (posOrig - posStart)
            printfn "posStart %i %i" posStart, leng
            printEmptyLine (posStart-1)
            printf "I"            
            printEmptyLine leng
            printLeftLeaf (posStart-1) (posOrig - posStart) posOrig
            
        
    let printStick width lenOrig=
        let pos, half, len  = createLeaf (width ,lenOrig)
        [1..len]
        |> List.map(fun _ -> 
            printEmptyLine pos
            printf "I"
            printEmptyLine pos
            printfn "") |> ignore
        printLeftLeaf pos len pos
        

    let main depth =
        if depth > 5 then
            printfn "maximum depth 5, you just tried %i please try again" depth
        printStick 100 16     
        printfn "\n The tree"

    main 4