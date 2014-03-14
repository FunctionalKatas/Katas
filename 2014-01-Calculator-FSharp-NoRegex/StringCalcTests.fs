namespace StringCalcTests

open System
open NUnit.Framework
open StringCalc

    type StringCalcTests() =
        
        [<Test>]
        member x.EmptyString() =
            Assert.AreEqual(0, Add "")

        [<Test>]
        member x.OneNumber() =
            Assert.AreEqual(1, Add "1")

        [<Test>]
        member x.MultipleNumbers() =
            Assert.AreEqual(3, Add "1,2")

        [<Test>]
        member x.NewlineDelimiter() =
            Assert.AreEqual(3, Add "1\n2")

        [<Test>]
        member x.MixedDelimiters() =
            Assert.AreEqual(6, Add "1\n2,3")

        [<Test>]
        member x.ConsecutiveDelimiters() =
            Assert.AreEqual(6, Add "1,\n2,3")

        [<Test>]
        member x.PreventNegatives() =
            try
                Add "1,-2,3,-4" |> ignore
                Assert.Fail "Didn't catch the negatives"
            with
                | NegativesFound negatives -> Assert.AreEqual([|-2;-4|], negatives)

        [<Test>]
        member x.IgnoreNumbersAbove1000() =
            Assert.AreEqual(2, Add "1001,2")

        [<Test>]
        member x.CustomDelimiter() =
            Assert.AreEqual(3, Add "//;\n1;2")

        [<Test>]
        member x.MultipleCustomDelimiters() =
            Assert.AreEqual(6, Add "//[:][|]\n1:2|3")
