module FunToolbox.Prelude

open System
open System.Threading
open System.Threading.Tasks
open System.Diagnostics
open System.Reflection

type CRAttribute = CompilationRepresentationAttribute

type RQAAttribute = RequireQualifiedAccessAttribute
let [<Literal>] ModuleSuffix = CompilationRepresentationFlags.ModuleSuffix

module Option =
    /// Return the value of the option, or elseValue when it is None.
    [<Obsolete("use Option.defaultValue")>]
    let inline orElseVal elseValue option = 
        match option with
        | Some v -> v
        | None -> elseValue

    /// Return the value of the option, call elseValue when it is None.
    [<Obsolete("use Option.defaultWith")>]
    let inline orElse elseValueF option = 
        match option with
        | Some v -> v
        | None -> elseValueF()

    // https://github.com/fsharp/fslang-design/issues/60#issuecomment-262224937
    let inline defaultValue value = 
        function
        | Some v -> v
        | None -> value

    let inline defaultWith f = 
        function
        | Some v -> v
        | None -> f()

    /// Returns Some () for true, and None for false
    let inline ofBool b = 
        if b then Some () else None

    let inline toBool o =
        match o with
        | Some _ -> true
        | None -> false

    /// Returns Some v if the value can be cast to the given type.
    let inline cast<'rt> (v: obj) : ('rt option) = 
        match v with
        | :? 'rt as v -> Some v
        | _ -> None

    /// returns None when an exception happens while computing the result.
    let inline tryWith (f: unit -> 'r) : ('r option) = 
        try Some <| f()
        with _ -> None

/// Equivalent to the <| operator, but with a more useful priority to separate funs, etc.
[<Obsolete("Use ^ instead")>]
[<DebuggerStepThrough>]
let inline (--) a b = a b
[<DebuggerStepThrough>]
let (^) = (<|)

// ordinal startsWith / endsWith for strings
type System.String with
    member this.startsWith part = 
        this.StartsWith(part, StringComparison.Ordinal)
    member this.endsWith part = 
        this.EndsWith(part, StringComparison.Ordinal)
    
module List =
    let rec revAndPrepend a l = 
        match a with
        | next :: rest -> revAndPrepend rest (next::l) 
        | [] -> l

module Seq = 

    /// Materialize sequence _and_ detach it from its source by converting it to an array.
    let inline materialize (s: 'e seq) : 'e seq =
        s |> Seq.toArray |> seq

    /// Evaluates the complete sequence right now and returns a sequence that points to an internal 
    /// array of the evaluation result. If the sequence _is_ a materialized collection already, like
    /// a _ list, a List<_>, or Array<_> it leaves it as it is.
    let inline ensureMaterialized (s: 'e seq) : 'e seq = 
        match s with
        | :? Array -> s
        | :? ('e list) -> s
        | :? System.Collections.Generic.List<'e> -> s
        | s ->
            s |> materialize

type System.Collections.Generic.List<'a> with
    member this.TakeAll() = 
        let r = this.ToArray()
        this.Clear()
        r

let inline flip f a b = f b a
let inline curry f a b = f (a,b)
let inline uncurry f (a,b) = f a b

/// A predicate & combinator.
let (<&>) f g = (fun x -> f x && g x)

let inline expect expected seen = 
    if expected <> seen then
        failwithf "internal error, unexpected state, expected %A, but seen %A" expected seen

module Result = 
    let inline bind c = function Ok r -> c r | Error e -> Error e

    /// Wrap a function into a result by capturing the exception the function may throw.
    /// tbd: I am not sure about the name here, and how its related to the monadic nature of the Result type.
    let inline capture (f: 'a -> 'b) = 
        fun p ->
            try Ok ^ f p
            with e -> Error e
    
//
// IDisposable
//

/// Convert a function to an IDisposable.Dispose(), useful for building return values
/// that can be used for `use` constructs.
let inline asDisposable f = 
    { new IDisposable with
        member __.Dispose() = f() }

let inline dispose (disposable: #IDisposable) = 
    disposable.Dispose()

module DisposeChain =
    type T() = 
        let mutable chain = []
        member this.Use (disp: #IDisposable) =
            chain <- (disp :> IDisposable) :: chain
            disp

        member this.Push (disp: #IDisposable) =
            chain <- (disp :> IDisposable) :: chain

        member this.Dispose() = 
            (this :> IDisposable).Dispose()

        interface IDisposable with
            member this.Dispose() =
                match chain with
                | [] -> ()
                | next::rest ->
                next.Dispose()
                chain <- rest
                (this :> IDisposable).Dispose()

let disposeChain() = 
    new DisposeChain.T()

//
// MailboxProcessor
//

type Agent<'t> = MailboxProcessor<'t>
type 't agent = Agent<'t>

module Agent =
    let create() : 't agent =
        Agent.Start ^ fun _ -> async.Return()

/// Extend AsyncBuilder with the option bind tasks without using Async.AwaitTask. 
/// For a sophisticated implementation: https://github.com/kekyo/FSharp.Control.FusionTasks
/// Note that the first overload is used for type inference.
type AsyncBuilder with
    
    [<DebuggerStepThrough>]
    member __.Source(a: Async<'r>) : Async<'r> = 
        a

    [<DebuggerStepThrough>]
    member __.Source(s: 'e seq) : 'e seq =
        s

    member __.Source(t: Task<'r>) : Async<'r> = 
        Async.AwaitTask t

    member __.Source(t: Task) : Async<unit> = 
        Async.AwaitTask t

//
// Async
//

type 'Result async = Async<'Result>

module Async =

    [<Obsolete("use Async.unit instead")>]
    let inline result r = async.Return r
    let inline unit r = async.Return r

    let inline bind f computation = async.Bind(computation, f)

    let inline map f = bind (f >> unit)

    let inline mapOption (computation: 'a -> Async<'b>) = function
        | None -> async.Return None
        | Some value -> computation value |> map Some

type Async with
    // http://www.fssnip.net/hx
    static member AwaitTask (t : Task<'T>, timeout : TimeSpan) =
        async {
            use cts = new CancellationTokenSource()
            use timer = Task.Delay (timeout, cts.Token)
            let! completed = Async.AwaitTask <| Task.WhenAny(t, timer)
            if completed = (t :> Task) then
                cts.Cancel ()
                let! result = Async.AwaitTask t
                return Some result
            else 
                return None
        }

    // http://www.fssnip.net/hx
    static member AwaitTask (t : Task, timeout : TimeSpan) =
        async {
            use cts = new CancellationTokenSource()
            use timer = Task.Delay (timeout, cts.Token)
            let! completed = Async.AwaitTask <| Task.WhenAny(t, timer)
            if completed = t then
                cts.Cancel ()
                do! Async.AwaitTask t
                return true
            else 
                return false
        }

    static member MapOption (workflow: 'a -> Async<'b>) =
        fun valueOpt ->
        match valueOpt with
        | None -> async { return None }
        | Some value ->
            async {
                let! r = workflow value
                return Some r
            }

    static member Return = async.Return

type MailboxProcessor<'t> with
    static member StartFold (f: 'state -> 'request -> 'state Async) initial = 
        MailboxProcessor.Start <| 
        fun mb -> 
        let rec loop state = async {
            let! request = mb.Receive()
            let! state = f state request
            return! loop state
        }
        loop initial

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

type Exception with
    /// A message that shows the exception message and the first
    /// exception message in case of an AggregateException or 
    /// a TargetInvocationException.
    member this.FirstMessage = 
        match this with
        | :? AggregateException as ae ->
            match Seq.tryHead ae.InnerExceptions with
            | Some e -> e.FirstMessage
            | None -> ae.Message
        | :? TargetInvocationException as ti ->
            match Option.ofObj ti.InnerException with
            | Some e -> e.FirstMessage
            | None -> ti.Message
        | e -> 
            e.Message

module Regex =

    open System.Text.RegularExpressions

    let internal processMatchResult (m: Match) =
        if m.Success 
        then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    // http://www.fssnip.net/29/title/Regular-expression-active-pattern
    let (|Match|_|) pattern input =
        Regex.Match(input, pattern, RegexOptions.CultureInvariant)
        |> processMatchResult

    let (|MatchIgnoreCase|_|) pattern input =
        Regex.Match(input, pattern, RegexOptions.CultureInvariant ||| RegexOptions.IgnoreCase)
        |> processMatchResult

[<assembly:AutoOpen("FunToolbox.Prelude")>]
do
()

