module internal MultiSet

    type MultiSet<'a when 'a : comparison> = M of Map<'a,uint32>

    let empty = M (Map.empty)

    let isEmpty (M(m)) = 
        Map.isEmpty m

    let size (M(m)) = 
        Map.fold (fun acc _ i -> i + acc) 0u m

    let contains a (M(m)) =
        Map.containsKey a m

    let numItems a (M(m)) =
        match Map.tryFind a m with
        |Some s -> s
        |None -> 0u

    let add a n (M(m)) =
       Map.add a n m |> M

    let addSingle a m =
        add a 1u m

    let remove a n (M(m)) =
        match Map.tryFind a m with
        |Some s when n >= s -> Map.remove a m |> M
        |Some s -> Map.add a (s-n) m |> M
        |None -> M(m)

    let removeSingle a m =
        remove a 1u m 
        
    let fold f acc (M(m)) = 
        Map.fold f acc m

    let foldBack f (M(m)) acc =
        Map.foldBack f m acc