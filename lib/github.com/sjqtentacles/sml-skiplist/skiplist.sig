(* skiplist.sig

   A probabilistic skip list providing a persistent (immutable) ordered map.

   The map is keyed by an ordered type supplied via the ORDERED signature.
   Levels are generated deterministically from an embedded PRNG seed advanced
   on each insert, so builds are reproducible and byte-identical across
   MLton and Poly/ML. *)

signature ORDERED =
sig
  type t
  val compare : t * t -> order
end

signature SKIPMAP =
sig
  type key
  type 'a map

  (* The empty map. *)
  val empty  : 'a map

  (* Insert (or overwrite) a binding; returns the updated map. *)
  val insert : 'a map -> key -> 'a -> 'a map

  (* Remove a binding if present; a no-op when the key is absent. *)
  val delete : 'a map -> key -> 'a map

  (* Look up the value bound to a key. *)
  val find   : 'a map -> key -> 'a option

  (* The binding with the smallest key, or NONE if empty. *)
  val min    : 'a map -> (key * 'a) option

  (* The binding with the largest key, or NONE if empty. *)
  val max    : 'a map -> (key * 'a) option

  (* All bindings in strictly ascending key order. *)
  val toList : 'a map -> (key * 'a) list

  (* The number of bindings. *)
  val size   : 'a map -> int
end
