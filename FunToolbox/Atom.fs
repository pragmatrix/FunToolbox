namespace FunToolbox

type 'v atom = private {
    mutable value: 'v
}

module Atom =
    let create v = { value = v }
    let swap (f: 'v -> 'v) (a: 'v atom) : 'v = 
        lock a (fun () -> a.value <- f a.value; a.value)
    let value a = 
        lock a (fun () -> a.value)
    let set v a = 
        a
        |> swap (fun _ -> v)
        |> ignore


