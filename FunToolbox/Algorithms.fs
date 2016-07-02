namespace FunToolbox.Algorithms

open FunToolbox.Prelude

/// Directed, acyclic graphs
module DAG = 

    exception CycleFoundException

    let sortTopologically (graph: Map<'t, 't list>) = 

        let dfs visited start_node = 
            let rec explore path visited node = 
                if Set.contains node path then raise CycleFoundException else
                if List.contains node visited then visited else     
                    let new_path = Set.add node path
                    let edges    = graph.TryFind node |> Option.orElse (fun () -> [])
                    let visited  = List.fold (explore new_path) visited edges
                    node :: visited
            in explore Set.empty visited start_node
      
        Seq.fold (fun visited (node,_) -> dfs visited node) [] (graph |> Map.toList)

    let ofEdges (edges: ('t * 't) list) =
        edges
        |> List.groupBy fst
        |> List.map (fun (k, v) -> k, v |> List.map snd)
        |> Map.ofList

    let ofList (l: ('t * 't list) list) = 
        l |> Map.ofList

