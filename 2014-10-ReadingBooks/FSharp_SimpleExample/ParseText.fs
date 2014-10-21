module ParseText

open System
open System.IO

// Extract Words (find whitespace)
let regExp = System.Text.RegularExpressions.Regex("\w+")

// Use regExp to create a Seq of words.
let ToWords str =
    seq { for m in regExp.Matches(str) -> m.Value }

// Read in all the words in a file and convert to lower case
let FileWords fileName =
    fileName
    |> File.ReadLines
    |> Seq.collect ToWords
    |> Seq.map (fun w -> w.ToLowerInvariant())

// Read all the words in all the files in a directory into a single Seq
// (allows for large books split over multiple files)
let DirWords dirName =
    dirName
    |> Directory.EnumerateFiles
    |> Seq.collect FileWords


// modify this to point to a folder with a book (in text files)
let book = DirWords @"\temp\Books\BookTitle";;

let topWords words = 
    words
    |> Seq.countBy id
    |> Seq.sortBy snd
    |> List.ofSeq
    |> List.rev

let topPairs words =
    words
    |> Seq.pairwise
    |> Seq.countBy id
    |> Seq.sortBy snd
    |> List.ofSeq
    |> List.rev


