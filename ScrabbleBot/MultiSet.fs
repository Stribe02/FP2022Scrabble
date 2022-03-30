module internal MultiSet
type MultiSet<'a> when 'a : comparison = M of Map<'a,uint32>

let empty : MultiSet<'a> = M Map.empty

let isEmpty (M m) = Map.isEmpty m

let size (M m) = Map.fold (fun acc _ v -> acc + v) 0u m

let contains (a:'a) (M m) = Map.containsKey a m

let numItems (a:'a) (M m)=
    match Map.tryFind a m with
        | Some a -> a
        | None -> 0u

let add (a: 'a) x (M m) = Map.add a ((numItems a (M m)) + x) m |> M

let addSingle (a: 'a) (M m) = add a 1u (M m)

let remove (a: 'a) (x: uint32) (M m) =
    match Map.tryFind a m with
    | Some v ->
        if v > x then Map.add a (v - x) m |> M  // return map
        else Map.remove a m |> M
    | None -> (M m)

let removeSingle (a: 'a) (M m) = remove a 1u (M m)

// 'b = key
// 'a = acc
// uint32 = value
// f takes the number of occurrences of each element as an argument rather than
// calling f that number of times
let fold (f: 'a -> 'b -> uint32 -> 'a) (acc: 'a) ((M m):MultiSet<'b>) =
    Map.fold f acc m
    
let foldBack (f: 'a -> uint32 -> 'b -> 'b) (M m) acc =
    Map.foldBack f m acc