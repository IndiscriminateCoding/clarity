## Clarity - functional programming library for OCaml
### Description

The goal of this project is to make pure functional programming idioms as useful as possible given OCaml's absence of higher-kinded types and typeclasses.

### Main features are:

* Standard "classes" like Functor-Applicative-Monad
* Concrete instances like Reader-Writer-State
* Useful data types like Either or These

### Design notes

* All concrete datatypes also have it's constructors defined as values where name is prefixed with underscore. Sometimes it's more convenient to use "curried", first-class version of a constructor, e.g. following two are equivalent:
```ocaml
let long  = List.map (fun x -> Some x) a
let short = List.map _Some x
```
* Applicative operator (<\*>) are "lazy" by it's second argument. This allows for an applicative to "fail-fast" and don't compute unneeded values. "Laziness" here is just (unit -> 'a) closure, so you can use function combinators from Fn module for convenience:
```ocaml
open Clarity

val serialize : int -> int -> string -> string
val idx : int option
val long_computation : int -> int Option.t
val title : string Option.t

open Fn
open Option

let res : string Option.t =
  map serialize idx
    <*> defer long_computation 1024
    <*> const title
```
* Right folds are also "lazy" by "accumulator" argument of a folding function. This allows for shortcut when function no more needs data. For example, here is 'any' function from Foldable module that checks if at least one element of a Foldable satisfies given predicate:
```ocaml
let any p = foldr (fun x a -> p x || a ()) false
```
* Monoids and Semigroups are defined in two versions - as module type and as value type. Module-based representation allows passing instances to functors (like Writer.Make, for example) while value-based allows to supply instances to functions like 'fold_map'. You can easily convert one representation to another using 'pack' and 'unpack' helpers.

### Documentation

You can find ocamldoc [here](https://indiscriminatecoding.github.io/clarity-docs/).

### Installation

    $ make && make install

