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
        
        /// Return the value of the option, or elseValue when it is None.
        let inline orElseConst elseValue option = 
            match option with
            | Some v -> v
            | None -> elseValue

        /// Returns Some () for true, and None for false
        let inline ofBool b = 
            if b then Some () else None

        /// Returns Some v if the value can be cast to the given type.
        let inline cast<'rt> (v: obj) : ('rt option) = 
            match v with
            | :? 'rt as v -> Some v
            | _ -> None

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

    let inline flip f a b = f b a          
    
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

    // https://github.com/fsprojects/FSharpx.Extras/blob/master/src/FSharpx.Extras/ComputationExpressions/Monad.fs
    /// The maybe monad.
    /// This monad is my own and uses an 'T option. Others generally make their own Maybe<'T> type from Option<'T>.
    /// The builder approach is from Matthew Podwysocki's excellent Creating Extended Builders series http://codebetter.com/blogs/matthew.podwysocki/archive/2010/01/18/much-ado-about-monads-creating-extended-builders.aspx.
    type MaybeBuilder() =
        member __.Return(x) = Some x
        member __.ReturnFrom(m: 'T option) = m
        member __.Bind(m, f) = Option.bind f m
        member __.Zero() = None
        member __.Combine(m, f) = Option.bind f m
        member __.Delay(f: unit -> _) = f
        member __.Run(f) = f()
        member this.TryWith(m, h) =
            try this.ReturnFrom(m)
            with e -> h e
        member this.TryFinally(m, compensation) =
            try this.ReturnFrom(m)
            finally compensation()
        member this.Using(res:#IDisposable, body) =
            this.TryFinally(body res, fun () -> match res with null -> () | disp -> disp.Dispose())
        member this.While(guard, f) =
            if not (guard()) then Some () else
            do f() |> ignore
            this.While(guard, f)
        member this.For(sequence:seq<_>, body) =
            this.Using(sequence.GetEnumerator(),
                fun enum -> this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current)))

    let maybe = MaybeBuilder()

[<assembly:AutoOpen("FunToolbox.Prelude")>]
do
    ()    

