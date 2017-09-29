module FunToolbox.Tests.SortedArrayTests

open FsUnit
open Xunit
open FunToolbox

[<Fact>]
let sortedArrayRangeMatchingTests() = 
    let array = 
        [0;1;2;3;4]
        |> SortedArray.ofSeq
        
    // end range, matching
    array
    |> SortedArray.range (1,4)
    |> Seq.toList 
    |> should equal [1;2;3;4]

    // begin range, matching
    array
    |> SortedArray.range (0,1)
    |> Seq.toList 
    |> should equal [0;1]

    // inner range, matching
    array
    |> SortedArray.range (1,3)
    |> Seq.toList 
    |> should equal [1;2;3]

    // too low, matching
    array
    |> SortedArray.range (-1,2)
    |> Seq.toList 
    |> should equal [0;1;2]
        
    // too high, matching
    array
    |> SortedArray.range (3,5)
    |> Seq.toList 
    |> should equal [3;4]

    // one element, matching
    array
    |> SortedArray.range (2,2)
    |> Seq.toList 
    |> should equal [2]

    // first element, matching
    array
    |> SortedArray.range (0,0)
    |> Seq.toList 
    |> should equal [0]

    // last element, matching
    array
    |> SortedArray.range (4,4)
    |> Seq.toList 
    |> should equal [4]

[<Fact>]
let sortedArrayRangeNonMatchingTests() = 
        
    let array = 
        [0;2;4;6;8] 
        |> SortedArray.ofSeq

    // end range
    array
    |> SortedArray.range (5,9)
    |> Seq.toList
    |> should equal [6;8]

    // begin range
    array
    |> SortedArray.range (-1,3)
    |> Seq.toList
    |> should equal [0;2]

    // inner range
    array
    |> SortedArray.range (3,7)
    |> Seq.toList
    |> should equal [4;6]

    // too low
    array
    |> SortedArray.range (-1,3)
    |> Seq.toList
    |> should equal [0;2]
        
    // too high
    array
    |> SortedArray.range (5,9)
    |> Seq.toList
    |> should equal [6;8]

    // one element
    array
    |> SortedArray.range (1,3)
    |> Seq.toList
    |> should equal [2]

    // first element
    array
    |> SortedArray.range (0,1)
    |> Seq.toList
    |> should equal [0]

    // last element
    array
    |> SortedArray.range (7,8)
    |> Seq.toList
    |> should equal [8]

[<Fact>]
let sortedArrayRangeOutOfRangeTests() = 

    let array = 
        [0;1;2;3;4]
        |> SortedArray.ofSeq

    // range completely too low
    array
    |> SortedArray.range (-2,-1)
    |> Seq.toList
    |> should be Empty

    // range completely too high
    array
    |> SortedArray.range (5,6)
    |> Seq.toList
    |> should be Empty

    // invalid range
    array
    |> SortedArray.range (2,1)
    |> Seq.toList
    |> should be Empty

    [] 
    |> SortedArray.ofSeq
    |> SortedArray.range(0,1)
    |> should be Empty

[<Theory>]
[<InlineData(-1, false)>]
[<InlineData(1, false)>]
[<InlineData(3, false)>]
[<InlineData(0, true)>]
[<InlineData(2, true)>]
let sortedArrayContains(value, containsIt: bool) = 
    let array = 
        [0;2]
        |> SortedArray.ofSeq

    array
    |> SortedArray.contains value
    |> should equal containsIt


[<Fact>]
let ``lower bound matching``() = 
    
    [
        [1;2;3]
        [1;2;2;3]
        [1;2;2;2;3]
        [1;2;2;2;2;3]
    ]
    |> List.iter ^ fun l ->
        l
        |> SortedArray.ofSeq
        |> SortedArray.lowerBound 2 
        |> should equal 1

    [
        []
        [1]
        [1;1]
        [1;1;1]
        [1;1;1;1]
        [1;1;1;1;1]
        [1;1;1;1;1;1]
    ]
    |> List.iter ^ fun l ->
        l
        |> SortedArray.ofSeq
        |> SortedArray.lowerBound 1
        |> should equal 0

[<Fact>]
let ``upper bound matching``() = 

    [
        [1;1;1;1;1;1]
        [0;1;1;1;1;1]
        [0;0;1;1;1;1]
        [0;0;0;1;1;1]
        [0;0;0;0;1;1]
        [0;0;0;0;0;1]
        [0;0;0;0;0;0]
    ]
    |> List.iter ^ fun l ->
        l
        |> SortedArray.ofSeq
        |> SortedArray.upperBound 1
        |> should equal 6

    [1;2;2;2;2;3]
    |> SortedArray.ofSeq
    |> SortedArray.upperBound 2
    |> should equal 5


[<Fact>]
let ``upper bound not matching``() = 

    [1;1;1;1;1;3;3;3;3;3]
    |> SortedArray.ofSeq
    |> SortedArray.upperBound 2
    |> should equal 5






