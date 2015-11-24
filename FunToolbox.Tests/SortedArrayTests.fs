namespace FunToolbox.Tests

open NUnit.Framework
open FsUnit

open FunToolbox

[<TestFixture>]
type SortedArrayTests() =

    [<Test>]
    member this.sortedArrayRangeMatchingTests() = 
        let array = 
            [0;1;2;3;4]
            |> SortedArray.ofSeq
        
        // end range, matching
        array
        |> SortedArray.range (1,4)
        |> should equal [1;2;3;4]

        // begin range, matching
        array
        |> SortedArray.range (0,1)
        |> should equal [0;1]

        // inner range, matching
        array
        |> SortedArray.range (1,3)
        |> should equal [1;2;3]

        // too low, matching
        array
        |> SortedArray.range (-1,2)
        |> should equal [0;1;2]
        
        // too high, matching
        array
        |> SortedArray.range (3,5)
        |> should equal [3;4]

        // one element, matching
        array
        |> SortedArray.range (2,2)
        |> should equal [2]

        // first element, matching
        array
        |> SortedArray.range (0,0)
        |> should equal [0]

        // last element, matching
        array
        |> SortedArray.range (4,4)
        |> should equal [4]

    [<Test>]
    member this.sortedArrayRangeNonMatchingTests() = 
        
        let array = 
            [0;2;4;6;8] 
            |> SortedArray.ofSeq

        // end range
        array
        |> SortedArray.range (5,9)
        |> should equal [6;8]

        // begin range
        array
        |> SortedArray.range (-1,3)
        |> should equal [0;2]

        // inner range
        array
        |> SortedArray.range (3,7)
        |> should equal [4;6]

        // too low
        array
        |> SortedArray.range (-1,3)
        |> should equal [0;2]
        
        // too high
        array
        |> SortedArray.range (5,9)
        |> should equal [6;8]

        // one element
        array
        |> SortedArray.range (1,3)
        |> should equal [2]

        // first element
        array
        |> SortedArray.range (0,1)
        |> should equal [0]

        // last element
        array
        |> SortedArray.range (7,8)
        |> should equal [8]

    [<Test>]
    member this.sortedArrayRangeOutOfRangeTests() = 

        let array = 
            [0;1;2;3;4]
            |> SortedArray.ofSeq

        // range completely too low
        array
        |> SortedArray.range (-2,-1)
        |> should equal []

        // range completely too high
        array
        |> SortedArray.range (5,6)
        |> should equal []

        // invalid range
        array
        |> SortedArray.range (2,1)
        |> should equal []

        [] 
        |> SortedArray.ofSeq
        |> SortedArray.range(0,1)
        |> should equal []

    [<Test>]
    member this.sortedArrayContains() = 
        let array = 
            [0;2]
            |> SortedArray.ofSeq

        array
        |> SortedArray.contains 3
        |> should equal false

        array
        |> SortedArray.contains 1
        |> should equal false

        array
        |> SortedArray.contains 0
        |> should equal true

        array
        |> SortedArray.contains -1
        |> should equal false

        array
        |> SortedArray.contains 2
        |> should equal true

