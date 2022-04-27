module internal Dict

    type Dict =
        | Node of Map<char, Dict> * bool

    let restOfWord (s:string) = s.[1..]

    let empty = fun() -> Node(Map.empty, false)

    let insert (word: string) (dict: Dict) =
        let rec aux =
            function
                |[], Node(m,b) -> Node (m, true)
                |firstChar::restOfWord, Node (m, b) ->
                    match m.TryFind firstChar with
                    | Some (Node (n', b')) ->
                        let node = Node(m, b')
                        let newMap = m.Add (firstChar, aux (restOfWord, empty()))
                        Node (newMap, b)
                    | None ->
                        let newMap = m.Add (firstChar, aux (restOfWord, empty()))
                        Node (newMap, b)
        aux (Seq.toList word, dict)
        
    let step (ch: char) (dict: Dict) =
        match dict with
        | Node(m,b) when m.ContainsKey ch ->
            let x = Map.find ch m
            match x with
            | Node(m', b') -> Some (b', x)
        | _ -> None

    let rec lookup word =
        function
            |Node(_, b) when word = "" -> b
            |Node(map, _) -> lookup (restOfWord word) (Map.find word.[0] map)
        
                