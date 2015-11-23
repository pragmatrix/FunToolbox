namespace FunToolbox.Tests

open NUnit.Framework
open FsUnit

open FunToolbox

[<TestFixture>]
type CollectionTests() =

    [<Test>]
    member this.sortedArrayMatchingTests() = 
        
        // end range, matching
        [|0;1;2;3;4|]
        |> SortedArray.slice (1,4)
        |> should equal [1;2;3;4]

        // begin range, matching
        [|0;1;2;3;4|]
        |> SortedArray.slice (0,1)
        |> should equal [0;1]

        // inner range, matching
        [|0;1;2;3;4|]
        |> SortedArray.slice (1,3)
        |> should equal [1;2;3]

        // too low, matching
        [|0;1;2;3;4|]
        |> SortedArray.slice (-1,2)
        |> should equal [0;1;2]
        
        // too high, matching
        [|0;1;2;3;4|]
        |> SortedArray.slice (3,5)
        |> should equal [3;4]

        // one element, matching
        [|0;1;2;3;4|]
        |> SortedArray.slice (2,2)
        |> should equal [2]

        // first element, matching
        [|0;1;2;3;4|]
        |> SortedArray.slice (0,0)
        |> should equal [0]

        // last element, matching
        [|0;1;2;3;4|]
        |> SortedArray.slice (4,4)
        |> should equal [4]

    [<Test>]
    member this.sortedNonMatchingTests() = 
        
        // end range
        [|0;2;4;6;8|]
        |> SortedArray.slice (5,9)
        |> should equal [6;8]

        // begin range
        [|0;2;4;6;8|]
        |> SortedArray.slice (-1,3)
        |> should equal [0;2]

        // inner range
        [|0;2;4;6;8|]
        |> SortedArray.slice (3,7)
        |> should equal [4;6]

        // too low
        [|0;2;4;6;8|]
        |> SortedArray.slice (-1,3)
        |> should equal [0;2]
        
        // too high
        [|0;2;4;6;8|]
        |> SortedArray.slice (5,9)
        |> should equal [6;8]

        // one element
        [|0;2;4;6;8|]
        |> SortedArray.slice (1,3)
        |> should equal [2]

        // first element
        [|0;2;4;6;8|]
        |> SortedArray.slice (0,1)
        |> should equal [0]

        // last element
        [|0;2;4;6;8|]
        |> SortedArray.slice (7,8)
        |> should equal [8]

    [<Test>]
    member this.sortedArrayOutOfRangeTests() = 

        // range completely too low
        [|0;1;2;3;4|]
        |> SortedArray.slice (-2,-1)
        |> should equal []

        // range completely too high
        [|0;1;2;3;4|]
        |> SortedArray.slice (5,6)
        |> should equal []

        // invalid range
        [|0;1;2;3;4|]
        |> SortedArray.slice (2,1)
        |> should equal []

        [||]
        |> SortedArray.slice(0,1)
        |> should equal []

        