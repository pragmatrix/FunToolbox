module FunToolbox.Tests.SubsetTests

open FsUnitTyped
open Xunit
open FunToolbox

[<Fact>]
let subset() =
    let nums = [0; 1; 2; 3; 4;5 ]
    let oddM2 n = if (n % 2) = 1 then Some 2 else None
    let mapping, subset = nums |> Subset.select oddM2
    let oddM2 =
        (nums, subset)
        |> Subset.integrate
             mapping
             (fun (n, o) ->
                match o with
                | Some(m) -> n * m
                | None -> n) 

    oddM2 |> shouldEqual [0; 2; 2; 6; 4; 10]

[<Fact>]
let ``Empty subset``() =
    let nums = [0; 1; 2; 3; 4; 5]
    let selector _ = None
    let mapping, subset = nums |> Subset.select selector
    let nums2 =
        (nums, subset)
        |> Subset.integrate
             mapping
             (fun (n, o) ->
                match o with
                | Some(m) -> n * m
                | None -> n) 
    nums2 |> shouldEqual nums

[<Fact>]
let ``Subset with all Some``() =
    let nums = [0; 1; 2; 3; 4; 5]
    let selector _ = Some 2
    let mapping, subset = nums |> Subset.select selector
    let m2 =
        (nums, subset)
        |> Subset.integrate
             mapping
             (fun (n, o) ->
                match o with
                | Some(m) -> n * m
                | None -> n) 
    m2 |> shouldEqual [0; 2; 4; 6; 8; 10]

[<Fact>]
let ``Integration with different superset``() =
    let nums = [0; 1; 2; 3; 4; 5]
    let oddM2 n = if (n % 2) = 1 then Some 2 else None
    
    let mapping, subset = nums |> Subset.select oddM2
    
    let oddM2 =
        ([0; 2; 4; 6; 8; 10], subset)
        |> Subset.integrate
            mapping  
            ^ fun (n, o) ->
                match o with
                | Some(m) -> n * m
                | None -> n

    oddM2 |> shouldEqual [0; 4; 4; 12; 8; 20]

[<Fact>]
let ``Integration with a superset of a different length fails``() =

    let nums = [0; 1; 2; 3; 4; 5]
    let oddM2 n = if (n % 2) = 1 then Some 2 else None
    
    let mapping, subset = nums |> Subset.select oddM2

    Assert.Throws<exn>(fun () ->    
        ([0; 2; 4; 6; 8], subset)
        |> Subset.integrate
            mapping    
            ^ fun (n, o) ->
                match o with
                | Some(m) -> n * m
                | None -> n
        |> ignore
    )

[<Fact>]
let ``Integration with a subset of a different length fails``() =
    let nums = [0; 1; 2; 3; 4; 5]
    let oddM2 n = if (n % 2) = 1 then Some 2 else None
    
    let mapping, subset = nums |> Subset.select oddM2

    Assert.Throws<exn>(fun () ->    
        (nums, [2; 3])
        |> Subset.integrate
            mapping    
            ^ fun (n, o) ->
                match o with
                | Some(m) -> n * m
                | None -> n
        |> ignore
    )

[<Fact>]
let ``empty superset``() =
    let nums = []
    let oddM2 n = if (n % 2) = 1 then Some 2 else None
    
    let mapping, subset = nums |> Subset.select oddM2

    let r =
        ([], subset)
        |> Subset.integrate
             mapping
             (fun (n, o) ->
                match o with
                | Some(m) -> n * m
                | None -> n) 

    r |> shouldBeEmpty
