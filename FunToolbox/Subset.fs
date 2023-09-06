[<RequireQualifiedAccess>]
module FunToolbox.Subset

/// A type representing a subset of a superset.
type 'a subset = {
    /// A subset of a superset.
    Subset: 'a list
    /// A list of flags that indicate which entries of the superset exists in the subset.
    Filter: bool list
}

/// Select a new subset from a superset.
///
/// - `superset` The superset.
/// - `selector` Chooses the subsets items for the superset's items.
let select (selector: 'i -> 'a option) (superset: 'i list) : 'a subset =

    let rec collect filter subset todo =
        match todo with
        | [] ->
            List.rev filter, List.rev subset
        | now::rest ->
            match selector now with
            | Some r -> collect (true::filter) (r::subset) rest
            | None -> collect (false::filter) subset rest

    let filter, subset =
        superset |> collect [] []

    {
        Subset = subset
        Filter = filter
    }        
    
/// Integrate the subset back to _a_ superset.
///
/// The superset do not need to be equal to the original superset that was used to create the subset, but it must have the same
/// length as the the original superset from which the subset got created.
let integrate (superset: 'i list) (apply: 'i * 'a option -> 'r) (subsetIn: 'a subset): 'r list =
    
    let rec integrate result filter subset todo =
        match filter, subset, todo with
        | true::fRest, s::sRest, input::rest ->
            let r = apply (input, Some(s))
            integrate (r::result) fRest sRest rest
        | false::fRest, subset, input::rest ->
            let r = (input, None) |> apply
            integrate (r::result) fRest subset rest
        | [], [], [] -> result |> List.rev
        // Error cases.
        | _ ->
            if List.length superset <> List.length subsetIn.Filter then
                failwith "Superset does not match length of original superset the subset was created from"
            else
                failwith "Internal error"

    superset
    |> integrate [] subsetIn.Filter subsetIn.Subset
