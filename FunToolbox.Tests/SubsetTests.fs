module FunToolbox.Tests.SubsetTests

open FsUnit
open Xunit
open FunToolbox

[<Fact>]
let ``subset``() =
    let nums = [0;1;2;3;4;5]
    let oddm2 n = if (n % 2) = 1 then Some 2 else None
    
    let subset = nums |> Subset.select oddm2
    
    let oddm2 =
        subset
        |> Subset.integrate
             nums
             (fun (n, o) ->
                match o with
                | Some(m) -> n * m
                | None -> n) 

    oddm2 |> should equal [0; 2; 2; 6; 4; 10]

[<Fact>]
let ``empty subset``() =
    let nums = [0;1;2;3;4;5]
    let selector n = None
    let subset = nums |> Subset.select selector
    let oddm2 =
        subset
        |> Subset.integrate
             nums
             (fun (n, o) ->
                match o with
                | Some(m) -> n * m
                | None -> n) 
    oddm2 |> should equal nums
