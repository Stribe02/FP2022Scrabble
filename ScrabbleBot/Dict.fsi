module internal Dict


type Dict = Node of Map<char, Dict> * bool
    val empty : unit -> Dict
    val insert : string -> Dict -> Dict
  //  val lookup : string -> Dict -> bool
  //  val step : char -> Dict -> (bool* Dict) option
