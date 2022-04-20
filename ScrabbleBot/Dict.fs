module internal Dict

type Dict = Node of Map<char, Dict> * bool

let empty = fun () -> Node(Map.empty, false)

let rec insert word (Node(map, b)) =
//    match word with
//    |"" -> Node(map, true)
//    |s ->
//        let firstChar = s.[0]
//        let otherChars = s.[1..]
//        match Map.tryFind firstChar with
//        |Some ->
//            let newNode = insert otherChars dict
//        |None -> 