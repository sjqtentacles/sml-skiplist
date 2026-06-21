# sml-skiplist

Probabilistic skip list ordered map and set functor in pure Standard ML

A persistent (immutable) ordered map keyed by any totally ordered type. Tower
heights are drawn from a geometric distribution (max level 16, p = 0.25) using a
deterministic [SplitMix64](https://github.com/sjqtentacles/sml-prng) PRNG seeded
with a fixed value, so builds are reproducible and byte-identical across MLton
and Poly/ML.

## Installation

```
smlpkg add github.com/sjqtentacles/sml-skiplist
smlpkg sync
```

Then include the library in your MLB file:

```
../lib/github.com/sjqtentacles/sml-skiplist/sources.mlb
```

## API

```sml
signature ORDERED =
sig
  type t
  val compare : t * t -> order
end

signature SKIPMAP =
sig
  type key
  type 'a map
  val empty  : 'a map
  val insert : 'a map -> key -> 'a -> 'a map
  val delete : 'a map -> key -> 'a map
  val find   : 'a map -> key -> 'a option
  val min    : 'a map -> (key * 'a) option
  val max    : 'a map -> (key * 'a) option
  val toList : 'a map -> (key * 'a) list    (* ascending *)
  val size   : 'a map -> int
end

functor SkipList (O : ORDERED) :> SKIPMAP where type key = O.t
```

## Usage

```sml
(* Instantiate over int keys. *)
structure M = SkipList (struct type t = int val compare = Int.compare end)

val m0 = M.empty
val m1 = M.insert m0 3 "three"
val m2 = M.insert m1 1 "one"
val m3 = M.insert m2 2 "two"

val () = if M.find m3 2 = SOME "two" then () else raise Fail "lookup"
val () = if M.find m3 9 = NONE       then () else raise Fail "absent"

val xs = M.toList m3      (* [(1,"one"), (2,"two"), (3,"three")] (ascending) *)
val n  = M.size m3        (* 3 *)
val lo = M.min m3         (* SOME (1, "one")   *)
val hi = M.max m3         (* SOME (3, "three") *)

val m4 = M.delete m3 2    (* removes the binding for key 2 *)
```

Inserting an existing key overwrites its value without growing the map;
deleting an absent key is a no-op. `toList` always returns bindings in strictly
ascending key order.

## Testing

```
make test       # MLton
make test-poly  # Poly/ML
```

Both targets run the same suite (1000-key scrambled-insert stress test, find /
delete / min / max / size coverage, and a sorted-oracle comparison) and must
report `0 failed` on both compilers.

## License

MIT
