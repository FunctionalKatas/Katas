namespace Luhn

module Seq =
    let rec cycle s = seq { yield! s; yield! cycle s}

    let rev s = s |> Array.ofSeq |> Array.rev |> Seq.ofArray