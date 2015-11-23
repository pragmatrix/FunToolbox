namespace FunToolbox

open System

module SortedArray = 
    
    /// Returns the sorted sequence ragne of values from low to high (including)
    let slice  (low: 'v, high: 'v) (sa: 'v array) : 'v seq =
        let first = Array.BinarySearch(sa, low)
        let first = if first < 0 then ~~~first else first
        if first = sa.Length then Seq.empty
        else
        let last = Array.BinarySearch(sa, high)
        if last >= 0 then
            seq {first..last}
        else
            seq {first..(~~~last)-1}
        |> Seq.map (fun i -> sa.[i])
