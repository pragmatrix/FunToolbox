namespace FunToolbox.Algorithms

open System.Linq
open FunToolbox.Prelude
open System.Collections.Generic

/// Directed, acyclic graphs
module DAG = 

    exception CycleFoundException

    type Graph<'t> = private Graph of ('t * 't list) list
    type 't graph = Graph<'t>

    let sortTopologically (Graph graph) = 

        let lookup = graph.ToDictionary((fun (k, _) -> k), (fun (_, v) -> v))
        let visited = HashSet<'t>()

        let dfs result start_node = 
            let rec explore path result node = 
                if visited.Contains node then result else     
                if Set.contains node path then raise CycleFoundException else
                    let newPath = Set.add node path
                    let edges = 
                        match lookup.TryGetValue node with
                        | true, edges -> edges
                        | false, _ -> []

                    let result  = List.fold (explore newPath) result edges
                    visited.Add node |> ignore
                    node :: result
            in explore Set.empty result start_node
      
        Seq.fold (fun result (node,_) -> dfs result node) [] graph

    let ofEdges (edges: ('t * 't) list) =
        edges
        |> List.groupBy fst
        |> List.map (fun (k, v) -> k, v |> List.map snd)
        |> Graph

    let ofList (l: ('t * 't list) list) = 
        l 
        |> List.groupBy fst
        |> List.map (fun (k, v) -> k, v |> List.collect snd)
        |> Graph

