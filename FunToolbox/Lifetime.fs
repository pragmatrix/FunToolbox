/// A module to manage object lifetimes.
///
/// The basic assumption is that "inner" lifetime are docked to "outer" lifetime scopes and
/// so their lifetime is combined into one handle that refers to the "inner" instace. When
/// This handle is disposed, the "inner" lifetime is disposed before the "outer" instance. 
/// Construction is from "outside" to "inside". Destruction is always the other way around.
/// This can be used with `use` construct in the following way given Outer() and Inner() return
/// IDisposable instances:
///
/// use outer = Outer() |> Lifetime.liftDisposable
/// use inner = Inner() |> Lifetime.liftDisposable |> Lifetime.dock outer
/// [initialization code]
/// /*return*/ inner |> Lifetime.detach
///
/// Note that `outer` and `inner` are lifetime handles. 
/// To access the instances of outer or inner, ".instance" has to be used.
///
/// Also note that Lifetime methods must only be used in the construction phase of a Lifetime
/// container, because they use mutable variables.
module FunToolbox.LifetimeExtensions

open System

[<RequireQualifiedAccess>]
module Lifetime = 
    
    type 't instance = 't * (unit -> unit)

    type 't handle = {
        // this does not need to be thread-safe owner changes should happen while
        // instances are being constructed!
        mutable _instance: 't instance
    } with
        interface IDisposable with
            member this.Dispose() = 
                this.dispose()
        member this.dispose() =
            this.destructor()

        member this.instance = 
            this._instance |> fst
        member this.destructor = 
            this._instance |> snd

    /// Returns a clone of the given handle and removes the destructor from the original.
    let detach (handle: 't handle) : 't handle = 
        let r = { _instance = handle._instance }
        // instance_ = handle.instance, handle.destructor }
        handle._instance <- handle.instance, id
        r

    /// Embed the lifetime of an inner instance to that of an outer one.
    /// The outer and inner handles are detached and the return value contains a
    /// handle to the inner instance that destructs the inner instance before the outer.
    let dock (outer: 'o handle) (inner: 'i handle) = 
        let outer = detach outer
        let inner = detach inner
        { _instance = inner.instance, fun() -> inner.dispose(); outer.dispose() }
        
    /// Convert a instance / destructor pair a lifetime handle.
    let lift v =  { _instance = v }

    /// Convert a IDisposable instance to a lifetime handle.
    let liftDisposable (instance: 't when 't :> IDisposable) =
        { _instance = instance, fun() -> instance.Dispose()}

    /// Detaches the handle and returns a new handle with the changed instance and the of 
    /// handle passed in.
    let map (f: 't -> 'i) (handle: 't handle) : 'i handle =
        let handle = detach handle
        (f handle.instance, handle.destructor)
        |> lift

[<AutoOpen("FunToolbox.LifetimeExtensions")>]
do ()     
    