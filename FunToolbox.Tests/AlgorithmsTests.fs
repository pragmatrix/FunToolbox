namespace FunToolbox.Tests

open NUnit.Framework
open FsUnit
open FunToolbox.Algorithms

type A = 
    | A
    | B 
    | C

type Recipe = Eggs | Milk | Wheat | Mix | Cook | Serve

[<TestFixture>]
type AlgorithmsTests() = 

    let sortEdges = Graph.ofEdges >> Graph.sortTopologically
    let graph = [ 
            Wheat, [Eggs;Milk;Mix] ;
            Milk,  [Mix] ;
            Cook,  [Serve] ;
            Eggs,  [Mix] ;
            Mix,   [Cook] ;
            Serve, [] ]
    
    [<Test>]
    member this.SortsTopologically() = 
        [(B, A); (C, A); (C, B)]
        |> sortEdges
        |> should equal [C; B; A]

    [<Test>]
    member this.SortsTopologically2() = 
        [(B, A); (C, A); (B, C)]
        |> sortEdges
        |> should equal [B; C; A]

    [<Test>]
    member this.SortsTopologically3() = 
        graph
        |> Graph.ofFans
        |> Graph.sortTopologically
        |> should equal [Wheat; Milk; Eggs; Mix; Cook; Serve]

    [<Test;ExpectedException(typeof<Graph.CycleFoundException>)>]
    member this.DetectsCycles() = 
        [(A, B); (C, A); (B, A)]
        |> sortEdges

    [<Test;ExpectedException(typeof<Graph.CycleFoundException>)>]
    member this.SelfCycle() = 
        [(A, A)]
        |> sortEdges

    [<Test>]
    member this.Empty() = 
        []
        |> sortEdges
        |> should equal []



        
            
