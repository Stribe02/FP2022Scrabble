module internal MultiSet


type MultiSet<'a> when 'a : comparison = M of Map<'a,uint32>
// empty : Multiset<'a> = creating an empty Multiset
val empty : MultiSet<'a>

//isEmpty : Multiset<'a> -> bool = true or false depending on if it's empty or not
val isEmpty : MultiSet<'a> -> bool

// return the size of s (sum of the number of occurrences of all elements in s)
// fold?
val size : MultiSet<'a> -> uint32

// an element a and a multiset s , returns true if a is an element of s and false otherwise
val contains : 'a -> MultiSet<'a> -> bool

// element a and a multiset s, returns how many occurrences of a there are in s.
val numItems : 'a -> MultiSet<'a> -> uint32

//given element a, number n, and multiset s, return the multiset that has a added to s n times
val add : 'a -> uint32 -> MultiSet<'a> -> MultiSet<'a>

// given element a and multiset s, return s with a single instanced of a added to the set
val addSingle : 'a -> MultiSet<'a> -> MultiSet<'a>

// given element a, number n, and multiset s, return multiset with n occurrences of a removed,
// but contains at least 0
val remove : 'a -> uint32 -> MultiSet<'a> -> MultiSet<'a>

// given element a and multiset s, return the multiset where a single occurrence of a has been removed from the set.
val removeSingle : 'a -> MultiSet<'a> -> MultiSet<'a>

//
val fold : ('a -> 'b -> uint32 -> 'a) -> 'a -> MultiSet<'b> -> 'a

val foldBack : ('a -> uint32 -> 'b -> 'b) -> MultiSet<'a> -> 'b -> 'b