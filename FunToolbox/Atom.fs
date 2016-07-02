namespace FunToolbox

/// An atom can only modify a value and return it. 'v -> 'v is the modification, which returns the
/// result (the 3rd 'v). 
type 'v atom = ('v -> 'v) -> 'v

module Atom =

    /// Create a new atom with a thread safe backing store.
    let create v = 
        let syncLock = obj()
        let mutable store = v
        fun f ->
            lock syncLock (fun () -> store <- f store; store)

    /// Apply a function to the atom and return the result (the current snapshot).
    let apply (f: 'v -> 'v) (a: atom<'v>) = a f
    
    /// Return the current value of a Atom (also implemented via apply)
    let snapshot a = 
        apply id a

    /// Reset the value to the given one (implemented via apply)
    let reset v a = 
        a
        |> apply (fun _ -> v)
        |> ignore

    /// Generate a limited capability to modify an atom. This also returns an atom.
    let limit (f: 's -> ('n -> 'n) -> 's) (s: 's atom) : ('n atom) =
        fun (m: 'n -> 'n) ->
            let mutable c = Unchecked.defaultof<'n>
            let mCapture v = c <- m v; c
            apply (fun s -> f s mCapture) s |> ignore
            c 

    /// Generate a function that returns the value of an atom.
    let snapshotOnly (a: 's atom) = 
        fun () ->
            a id


