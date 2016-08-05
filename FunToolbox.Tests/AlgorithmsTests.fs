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
    member __.SortsTopologically() = 
        [(B, A); (C, A); (C, B)]
        |> sortEdges
        |> should equal [C; B; A]

    [<Test>]
    member __.SortsTopologically2() = 
        [(B, A); (C, A); (B, C)]
        |> sortEdges
        |> should equal [B; C; A]

    [<Test>]
    member __.SortsTopologically3() = 
        graph
        |> Graph.ofFans
        |> Graph.sortTopologically
        |> should equal [Wheat; Milk; Eggs; Mix; Cook; Serve]

    [<Test>]
    member __.DetectsCycles() = 
        fun () ->
            [(A, B); (C, A); (B, A)]
            |> sortEdges
            |> ignore
        |> should throw typeof<Graph.CycleFoundException>

    [<Test>]
    member __.SelfCycle() = 
        fun () -> 
            [(A, A)]
            |> sortEdges
            |> ignore
        |> should throw typeof<Graph.CycleFoundException>

    [<Test>]
    member this.Empty() = 
        []
        |> sortEdges
        |> should equal []



        
            
