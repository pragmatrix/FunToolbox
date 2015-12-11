namespace FunToolbox

module Prelude =

    open System

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Option =

        /// Return the value of the option, if it is Some v, or elseValue when it is None.
        let inline orElse elseValue option = 
            match option with
            | Some v -> v
            | None -> elseValue

    /// Equivalent to the <| operator, but with a more useful priority to separate funs, etc.
    let inline (--) a b = a b

    // ordinal startsWith / endsWith for strings
    type System.String with
        member this.startsWith part = 
            this.StartsWith(part, StringComparison.Ordinal)
        member this.endsWith part = 
            this.EndsWith(part, StringComparison.Ordinal)
    
    module List =
        let inline flatten l = List.collect id l

    module Seq = 
        let inline flatten s = Seq.collect id s

    module Array = 
        let inline flatten a = Array.collect id a
        
[<assembly:AutoOpen("FunToolbox.Prelude")>]
do
    ()    

