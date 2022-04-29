module internal Dict

type Dict = Node of Map<char, bool * Dict>

let firstChar (s: string) = s.[0]
let restOfWord (s: string) = s.[1..]

let empty = fun () -> Node(Map.empty)

let insert (word: string) (dict: Dict) =
    let rec aux (w:string) (node: Dict) =
        match node with
        | Node n when w.Length = 1 && n.ContainsKey (firstChar w) = false -> Node(Map.add (firstChar w) (true, empty()) n)
        | Node n when w.Length = 1 -> Node(Map.add (firstChar w) (true, snd(Map.find (firstChar w) n)) n)
        | Node n when n.ContainsKey (firstChar w) -> Node (Map.add (firstChar w) (fst(Map.find (firstChar w) n), aux (restOfWord w) (snd(Map.find (firstChar w) n))) n)
        | Node n when n.ContainsKey (firstChar w) = false -> Node (Map.add (firstChar w) (false, aux (restOfWord w) (empty())) n)
    aux word dict
    
let lookup (word: string) (node: Dict) =
    let rec aux (w:string) (nod: Dict) =
        match nod with
        | Node n when w.Length = 1 -> Map.tryFind (firstChar w) n |>
            function
            | Some n -> true
            | None -> false
        | Node n when n.ContainsKey (firstChar w) -> aux (restOfWord w) (snd(Map.find (firstChar w) n))
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