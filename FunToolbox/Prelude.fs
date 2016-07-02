namespace FunToolbox

open System
open System.Threading.Tasks

module Prelude =

    type CRAttribute = CompilationRepresentationAttribute

    [<Literal>]
    let ModuleSuffix = CompilationRepresentationFlags.ModuleSuffix

    [<CR(ModuleSuffix)>]
    module Option =

        /// Return the value of the option, call elseValue when it is None.
        let inline orElse elseValueF option = 
            match option with
            | Some v -> v
            | None -> elseValueF()

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
        let rec revAndPrepend a l = 
            match a with
            | next :: rest -> revAndPrepend rest (next::l) 
            | [] -> l

    module Seq = 
        let inline flatten s = Seq.collect id s

    module Array = 
        let inline flatten a = Array.collect id a

    type Agent<'t> = MailboxProcessor<'t>
    type 't agent = Agent<'t>

    /// Convert a function to an IDisposable.Dispose(), useful for building return values
    /// that can be used for `use` constructs.
    let asDisposable f = 
        { new IDisposable with
            member this.Dispose() = f() }

    /// Extend AsyncBuilder with the option bind tasks without using Async.AwaitTask. 
    /// This is experimental, so only Bind is supported for now.
    type AsyncBuilder with
        member this.Bind(t: Task<'t>, f: 't -> Async<'r>) : Async<'r> = 
            this.Bind(t |> Async.AwaitTask, f)
        member this.Bind(t: Task, f: unit -> Async<'r>) : Async<'r> = 
            this.Bind(t |> Async.AwaitTask, f)

        member this.ReturnFrom(t : Task<'r>) : Async<'r> = 
            t |> Async.AwaitTask
        member this.ReturnFrom(t : Task) : Async<unit> = 
            t |> Async.AwaitTask

    /// A predicate & combinator.
    let (<&>) f g = (fun x -> f x && g x)

[<assembly:AutoOpen("FunToolbox.Prelude")>]
do
    ()    

