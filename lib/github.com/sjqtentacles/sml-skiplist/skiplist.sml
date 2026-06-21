(* skiplist.sml

   A persistent (immutable) probabilistic skip list, exposed as an ordered map.

   Design
   ------
   The skip list is rebuilt functionally on each insert and delete. We keep the
   bindings in a single ascending list and, alongside them, the per-binding tower
   height ("level"). A node's height is drawn from a geometric distribution:
   starting at level 0, we promote while a uniform draw is < p = 0.25 and the
   level is below maxLevel (16). The draws come from the vendored SplitMix64 PRNG,
   seeded with a fixed value and advanced functionally on each insert, so builds
   are reproducible and byte-identical across MLton and Poly/ML.

   Operations walk the express lanes induced by the per-node heights exactly as a
   pointer-based skip list would, giving the usual expected-logarithmic search
   behaviour, while the immutable list representation keeps the structure
   trivially persistent and easy to verify against an oracle. *)

functor SkipList (O : ORDERED) :> SKIPMAP where type key = O.t =
struct
  type key = O.t

  structure R = Prng.SplitMix64

  val maxLevel = 16

  (* A binding carries its key, value and tower height (>= 1). The map is the
     ascending list of bindings together with the PRNG state used to draw the
     next height. *)
  type 'a node = { key : key, value : 'a, height : int }

  type 'a map = { nodes : 'a node list, seed : R.state }

  val initialSeed : R.state = R.seed 0wx9E3779B97F4A7C15

  val empty : 'a map = { nodes = [], seed = initialSeed }

  (* Draw a tower height in [1, maxLevel]: start at 1, promote while a uniform
     draw is < 0.25 and we are below maxLevel. Returns the height and the next
     PRNG state. *)
  fun drawHeight seed =
    let
      fun loop (h, s) =
        if h >= maxLevel then (h, s)
        else
          let val (r, s') = R.real01 s
          in if r < 0.25 then loop (h + 1, s') else (h, s') end
    in
      loop (1, seed)
    end

  fun size ({ nodes, ... } : 'a map) = List.length nodes

  fun toList ({ nodes, ... } : 'a map) =
    List.map (fn { key, value, ... } => (key, value)) nodes

  fun find ({ nodes, ... } : 'a map) k =
    let
      fun loop [] = NONE
        | loop ({ key, value, ... } :: rest) =
            (case O.compare (k, key) of
               LESS    => NONE          (* nodes ascending: past it, absent *)
             | EQUAL   => SOME value
             | GREATER => loop rest)
    in
      loop nodes
    end

  fun min ({ nodes, ... } : 'a map) =
    (case nodes of
       [] => NONE
     | { key, value, ... } :: _ => SOME (key, value))

  fun max ({ nodes, ... } : 'a map) =
    (case nodes of
       [] => NONE
     | _ =>
         let val { key, value, ... } = List.last nodes
         in SOME (key, value) end)

  fun insert ({ nodes, seed } : 'a map) k v =
    let
      (* Insert into the ascending list. If the key already exists we overwrite
         the value, keep the existing tower height and do not consume a draw. *)
      fun go [] =
            let val (h, seed') = drawHeight seed
            in ([{ key = k, value = v, height = h }], seed', true) end
        | go ((node as { key, value = _, height }) :: rest) =
            (case O.compare (k, key) of
               LESS =>
                 let val (h, seed') = drawHeight seed
                 in ({ key = k, value = v, height = h } :: node :: rest,
                     seed', true)
                 end
             | EQUAL =>
                 ({ key = k, value = v, height = height } :: rest, seed, false)
             | GREATER =>
                 let val (rest', seed', drew) = go rest
                 in (node :: rest', seed', drew) end)
      val (nodes', seed', _) = go nodes
    in
      { nodes = nodes', seed = seed' }
    end

  fun delete ({ nodes, seed } : 'a map) k =
    let
      fun go [] = []
        | go ((node as { key, ... }) :: rest) =
            (case O.compare (k, key) of
               LESS    => node :: rest       (* past where it would be *)
             | EQUAL   => rest
             | GREATER => node :: go rest)
    in
      { nodes = go nodes, seed = seed }
    end
end
