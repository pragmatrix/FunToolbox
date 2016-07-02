namespace FunToolbox.Algorithms

open System.Linq
open FunToolbox.Prelude
open System.Collections.Generic

/// A directed graph
type Graph<'t> = private Graph of ('t * 't list) list
type 't graph = Graph<'t>

[<CR(ModuleSuffix)>]
module Graph = 

    exception CycleFoundException

    let sortTopologically (Graph graph) = 

        // use a dictionary instead of a map, so that we don't need the comparison constraint, and
        // stay linear in performance.
        let lookup = graph.ToDictionary(fst, snd)
        let visited = HashSet<'t>()

        let dfs result start_node = 

            // there is only one path when we run a depth first search, 
            // so we can optimize this by using a mutable HashSet, instead of Set or a List. 
            // This also this avoids the comparison constraint that a Set would introduce.
            let path = HashSet<'t>()

            let rec explore result node = 
                if visited.Contains node then result else     
                if path.Contains node then raise CycleFoundException else
                    path.Add node |> ignore
                    let edges = 
                        match lookup.TryGetValue node with
                        | true, edges -> edges
                        | false, _ -> []
                    let result = List.fold explore result edges
                    visited.Add node |> ignore
                    node :: result
            in explore result start_node
      
        Seq.fold (fun result (node,_) -> dfs result node) [] graph

    let ofEdges (edges: ('t * 't) list) =
        edges
        |> List.groupBy fst
        |> List.map (fun (k, v) -> k, v |> List.map snd)
        |> Graph

    let ofFans (fans: ('t * 't list) list) = 
        fans 
        |> List.groupBy fst
        |> List.map (fun (k, v) -> k, v |> List.collect snd)
        |> Graph

