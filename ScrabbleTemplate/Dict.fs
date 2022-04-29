module internal Dict

type Dict = Node of Map<char, bool * Dict>

let restOfWord (s: string) = s.[1..]

let empty = fun () -> Node(Map.empty)

let insert (word: string) (dict: Dict) =
    let rec aux (w:string) (node: Dict) =
        match node with
        | Node n when w.Length = 1 && n.ContainsKey w.[0] = false -> Node(Map.add (w.[0]) (true, empty()) n)
        | Node n when w.Length = 1 -> Node(Map.add (w.[0]) (true, snd(Map.find w.[0] n)) n)
        | Node n when n.ContainsKey w.[0] -> Node (Map.add (w.[0]) (fst(Map.find w.[0] n), aux (restOfWord w) (snd(Map.find w.[0] n))) n)
        | Node n when n.ContainsKey w.[0] = false -> Node (Map.add w.[0] (false, aux (restOfWord w) (empty())) n)
    aux word dict
    
let lookup (word: string) (node: Dict) =
    let rec aux (w:string) (nod: Dict) =
        match nod with
        | Node n when w.Length = 1 -> Map.tryFind w.[0] n |>
            function
            | Some n -> true
            | None -> false
        | Node n when n.ContainsKey w.[0] -> aux (restOfWord w) (snd(Map.find w.[0] n))
        | Node n -> false
    aux word node

let step (c : char) (node : Dict) = 
        let rec inner (c : char) (nod : Dict) = 
            match nod with
            | Node x when x.ContainsKey c  = false -> None  
            | Node x -> Map.tryFind c x |>
                function
                | Some (b, dict) -> 
                    match dict with
                    | Node d when b -> Some(b, dict)
                    | _ -> Some(b, dict)
                | None -> None
        inner c node