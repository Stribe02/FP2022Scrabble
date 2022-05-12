module ScrabbleBot.Helper

type coord = int * int
type tile = char * int
type piece = uint32 * tile
type move = (coord * piece) list

