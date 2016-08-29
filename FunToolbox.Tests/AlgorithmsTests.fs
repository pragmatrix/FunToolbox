module FunToolbox.Tests.AlgorithmsTests

open FsUnit
open Xunit
open FunToolbox.Algorithms

type A = 
    | A
    | B 
    | C

type Recipe = Eggs | Milk | Wheat | Mix | Cook | Serve

let sortEdges = Graph.ofEdges >> Graph.sortTopologically
let graph = [ 
    Wheat, [Eggs;Milk;Mix] ;
    Milk,  [Mix] ;
    Cook,  [Serve] ;
    Eggs,  [Mix] ;
    Mix,   [Cook] ;
    Serve, [] ]
    
[<Fact>]
let sortsTopologically() = 
    [(B, A); (C, A); (C, B)]
    |> sortEdges
    |> should equal [C; B; A]

[<Fact>]
let sortsTopologically2() = 
    [(B, A); (C, A); (B, C)]
    |> sortEdges
    |> should equal [B; C; A]

[<Fact>]
let sortsTopologically3() = 
    graph
    |> Graph.ofFans
    |> Graph.sortTopologically
    |> should equal [Wheat; Milk; Eggs; Mix; Cook; Serve]

[<Fact>]
let detectsCycles() = 
    fun () ->
        [(A, B); (C, A); (B, A)]
        |> sortEdges
        |> ignore
    |> should throw typeof<Graph.CycleFoundException>

[<Fact>]
let selfCycle() = 
    fun () -> 
        [(A, A)]
        |> sortEdges
        |> ignore
    |> should throw typeof<Graph.CycleFoundException>

[<Fact>]
let empty() = 
    []
    |> sortEdges
    |> should be Empty



        
            
