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
 
    [<RequireQualifiedAccess>]
    module Lifetime =
        /// A disposable instance that can be used inside a 'use' code block.
        type ScopedInstance<'t> = {
            destructor: unit -> unit
            instance : 't
        }
        with
            interface IDisposable with
                member this.Dispose() = this.dispose()
            member this.dispose() = this.destructor()

        /// Create a scoped and IDisposable record that transports another instance 
        /// and a destructor that is called when Dispose() is invoked.
        let scoped destructor instance  = {
                instance = instance
                destructor = destructor
            }

        /// Create a scoped lifetime instance that owns another in term of the lifetime. 
        /// The owned scope is disposed after the inner scope. 
        let owns (owned: ScopedInstance<_>) (scoped: ScopedInstance<_>) = {
                instance = scoped.instance
                destructor = 
                    fun () ->
                        scoped.dispose()
                        owned.dispose()
            }


[<assembly:AutoOpen("FunToolbox.Prelude")>]
do
    ()    

