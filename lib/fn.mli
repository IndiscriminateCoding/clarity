val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
external id : 'a -> 'a = "%identity"
val const : 'a -> 'b -> 'a
val compose : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
val defer : ('a -> 'b) -> 'a -> 'c -> 'b
val ( @! ) : ('a -> 'b) -> 'a -> 'c -> 'b

