// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

[<EntryPoint>]
let main argv = 
    //printfn "%A" argv

    let sieve max =
        let notPrime : bool array = Array.zeroCreate (max + 1)

        let isPrime n = not notPrime.[n]

        notPrime.[0] <- true
        notPrime.[1] <- true

        let setNotPrimes n =
            for i in {n * n..n..max} do
                notPrime.[i] <- true

        setNotPrimes 2

        let maxCheck = int( System.Math.Floor( System.Math.Sqrt (double(max))))

        for testPrime in {3..2..maxCheck} do
            if isPrime testPrime then
                setNotPrimes testPrime

        isPrime

    let max = 110000

    let isThisPrime = sieve max

    {0..max}
    |> Seq.where isThisPrime
    |> Seq.skipWhile (fun x -> x < 100000)
    |> Seq.skip 43
    |> Seq.take 5
    |> Seq.iteri (fun i x -> printfn "%d %A" i x)

    System.Console.ReadKey true |> ignore

    0 // return an integer exit code
