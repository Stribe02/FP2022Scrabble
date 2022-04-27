module internal Dict

type Dict =
    | Node of Map<char, Dict> * bool
    | Leaf of bool

let restOfWord (s:string) = s.[1..]

let empty = fun() -> Node(Map.empty, false)

let insert (word: string) (dict: Dict) =
    let rec aux =
        function
            |([], Node(m,b)) -> Node (m, true)
            |(c::tail, Node (m, b)) ->
                match m.TryFind c with
                | Some (Node (n', b')) ->
                    let node = Node (m, b')
                    let newMap = m.Add (c, aux (tail, empty()))
                    Node (newMap, b)
                | None ->
                    let newMap = m.Add (c, aux (tail, empty()))
                    Node (newMap, b)
    aux (Seq.toList word, dict)

let rec lookup word =
    function
        |Node(_, b) when word = "" -> b
        |Node(map, _) -> lookup (restOfWord word) (Map.find word.[0] map)