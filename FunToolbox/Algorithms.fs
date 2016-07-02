namespace FunToolbox.Algorithms

open FunToolbox.Prelude
open System.Collections.Generic

/// Directed, acyclic graphs
module DAG = 

    exception CycleFoundException

    let sortTopologically (graph: Map<'t, 't list>) = 

        let visited = HashSet<'t>()

        let dfs result start_node = 
            let rec explore path result node = 
                if Set.contains node path then raise CycleFoundException else
                if visited.Contains node then result else     
                    let newPath = Set.add node path
                    let edges = graph.TryFind node |> Option.orElse (fun () -> [])
                    let result  = List.fold (explore newPath) result edges
                    visited.Add node |> ignore
                    node :: result
            in explore Set.empty result start_node
      
        Seq.fold (fun result (node,_) -> dfs result node) [] (graph |> Map.toList)

    let ofEdges (edges: ('t * 't) list) =
        edges
        |> List.groupBy fst
        |> List.map (fun (k, v) -> k, v |> List.map snd)
        |> Map.ofList

    let ofList (l: ('t * 't list) list) = 
        l |> Map.ofList

