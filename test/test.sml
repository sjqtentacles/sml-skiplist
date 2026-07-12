(* test.sml

   Tests for sml-skiplist. The map is instantiated over int keys and exercised
   against a sorted-assoc-list oracle. *)

structure M = SkipList (struct type t = int val compare = Int.compare end)

structure Tests =
struct
  open Harness

  val N = 1000

  (* Scrambled, deduplicated key order: i*7 mod 1000 for i in [0,N). *)
  fun scrambledKeys () =
    let
      fun loop (i, seen, acc) =
        if i >= N then List.rev acc
        else
          let val k = (i * 7) mod N
          in if List.exists (fn x => x = k) seen
             then loop (i + 1, seen, acc)
             else loop (i + 1, k :: seen, k :: acc)
          end
    in
      loop (0, [], [])
    end

  (* Value associated with a key (arbitrary but deterministic). *)
  fun valOf k = k * 3 + 1

  fun buildFull () =
    List.foldl (fn (k, m) => M.insert m k (valOf k)) M.empty (scrambledKeys ())

  fun cmpPair ((k1, _), (k2, _)) = Int.compare (k1, k2)

  (* Portable stable-enough merge sort (works on MLton and Poly/ML). *)
  fun sortBy cmp xs =
    let
      fun merge ([], ys) = ys
        | merge (xs, []) = xs
        | merge (x :: xs, y :: ys) =
            (case cmp (x, y) of
               GREATER => y :: merge (x :: xs, ys)
             | _       => x :: merge (xs, y :: ys))
      fun split [] = ([], [])
        | split [x] = ([x], [])
        | split (x :: y :: rest) =
            let val (a, b) = split rest in (x :: a, y :: b) end
      fun msort [] = []
        | msort [x] = [x]
        | msort zs =
            let val (a, b) = split zs
            in merge (msort a, msort b) end
    in
      msort xs
    end

  fun isAscending [] = true
    | isAscending [_] = true
    | isAscending ((k1, _) :: (rest as (k2, _) :: _)) =
        k1 < k2 andalso isAscending rest

  fun run () =
    let
      val keys = scrambledKeys ()
      val sortedKeys = sortBy Int.compare keys
      val m = buildFull ()

      (* Section 1: insertion + ascending toList *)
      val () = section "insertion and ordering"
      val () = checkInt "inserted count" (List.length keys, M.size m)
      val () = check "toList keys strictly ascending" (isAscending (M.toList m))
      val () = checkIntList "toList keys = sorted keys"
                 (sortedKeys, List.map #1 (M.toList m))

      (* Section 2: find present / absent *)
      val () = section "find"
      val () = check "find every inserted key = SOME value"
                 (List.all (fn k => M.find m k = SOME (valOf k)) keys)
      val () = checkBool "find absent key (1000) = NONE"
                 (true, M.find m 1000 = NONE)
      val () = checkBool "find absent key (~5) = NONE"
                 (true, M.find m ~5 = NONE)

      (* Section 3: delete 500 then re-check *)
      val () = section "delete"
      val toDelete = List.take (sortedKeys, 500)
      val remaining = List.drop (sortedKeys, 500)
      val m2 = List.foldl (fn (k, acc) => M.delete acc k) m toDelete
      val () = checkInt "size after deleting 500"
                 (List.length keys - 500, M.size m2)
      val () = check "deleted keys = NONE"
                 (List.all (fn k => M.find m2 k = NONE) toDelete)
      val () = check "remaining keys = SOME value"
                 (List.all (fn k => M.find m2 k = SOME (valOf k)) remaining)
      val () = check "delete absent key is a no-op"
                 (M.size (M.delete m2 999999) = M.size m2)

      (* Section 4: min / max after mixed insert/delete *)
      val () = section "min and max"
      val () = checkBool "min of empty = NONE" (true, M.min M.empty = NONE)
      val () = checkBool "max of empty = NONE" (true, M.max M.empty = NONE)
      val minK = hd remaining
      val maxK = List.last remaining
      val () = checkBool ("min = " ^ Int.toString minK)
                 (true, M.min m2 = SOME (minK, valOf minK))
      val () = checkBool ("max = " ^ Int.toString maxK)
                 (true, M.max m2 = SOME (maxK, valOf maxK))
      (* re-insert a clearly-smaller and clearly-larger key *)
      val m3 = M.insert (M.insert m2 ~100 (valOf ~100)) 5000 (valOf 5000)
      val () = checkBool "min after inserting ~100"
                 (true, M.min m3 = SOME (~100, valOf ~100))
      val () = checkBool "max after inserting 5000"
                 (true, M.max m3 = SOME (5000, valOf 5000))

      (* Section 5: size *)
      val () = section "size"
      val () = checkInt "empty size = 0" (0, M.size M.empty)
      val () = checkInt "size of single insert" (1, M.size (M.insert M.empty 42 0))
      val () = checkInt "overwrite does not grow size"
                 (1, M.size (M.insert (M.insert M.empty 42 0) 42 99))
      val () = checkBool "overwrite updates value"
                 (true, M.find (M.insert (M.insert M.empty 42 0) 42 99) 42 = SOME 99)
      val () = checkInt "size after re-inserts = remaining + 2"
                 (List.length remaining + 2, M.size m3)

      (* Section 6: oracle comparison *)
      val () = section "oracle"
      val oraclePairs =
        sortBy cmpPair (List.map (fn k => (k, valOf k)) keys)
      val () = check "toList equals sorted (key,val) oracle"
                 (oraclePairs = M.toList m)
      (* oracle after deletes *)
      val oracleAfter =
        List.filter (fn (k, _) => not (List.exists (fn d => d = k) toDelete))
          oraclePairs
      val () = check "toList equals oracle after deletes"
                 (oracleAfter = M.toList m2)

      (* Section 7: properties (sml-check) *)
      val () = section "properties (sml-check)"

      val smallInt = Check.choose (~1000, 1000)
      val genPair = Check.tuple2 (smallInt, smallInt)
      val genList = Check.listOf genPair

      fun showPair (k, v) = "(" ^ Int.toString k ^ "," ^ Int.toString v ^ ")"
      fun showPairList xs = "[" ^ String.concatWith "," (List.map showPair xs) ^ "]"
      fun showIntList xs = "[" ^ String.concatWith "," (List.map Int.toString xs) ^ "]"

      fun dedupKeys ks =
        List.foldr
          (fn (k, acc) => if List.exists (fn k' => k' = k) acc then acc else k :: acc)
          [] ks

      fun buildFromPairs xs = List.foldl (fn ((k, v), m) => M.insert m k v) M.empty xs

      (* insert-then-find returns the inserted value. *)
      val () =
        Harness.check "prop: insert-then-find returns the inserted value"
          (case Check.quickCheck
                  (Check.forAll
                     (Check.tuple3 (genList, smallInt, smallInt))
                     (fn (xs, k, v) => showPairList xs ^ " k=" ^ Int.toString k
                                        ^ " v=" ^ Int.toString v)
                     (fn (xs, k, v) =>
                        let val m = buildFromPairs xs
                            val m' = M.insert m k v
                        in M.find m' k = SOME v end)) of
               Check.Passed _ => true
             | Check.Failed _ => false)

      (* delete-then-find returns NONE, whether or not the key was present. *)
      val () =
        Harness.check "prop: delete-then-find returns NONE"
          (case Check.quickCheck
                  (Check.forAll
                     (Check.tuple2 (genList, smallInt))
                     (fn (xs, k) => showPairList xs ^ " k=" ^ Int.toString k)
                     (fn (xs, k) =>
                        let val m = buildFromPairs xs
                            val m' = M.delete m k
                        in M.find m' k = NONE end)) of
               Check.Passed _ => true
             | Check.Failed _ => false)

      (* toList built from any insert list comes back in strictly ascending
         key order. *)
      val () =
        Harness.check "prop: toList keys are strictly ascending"
          (case Check.quickCheck
                  (Check.forAll genList showPairList
                     (fn xs => isAscending (M.toList (buildFromPairs xs)))) of
               Check.Passed _ => true
             | Check.Failed _ => false)

      (* inserting a set of distinct keys yields a map of exactly that size. *)
      val () =
        Harness.check "prop: size after inserting N distinct keys = N"
          (case Check.quickCheck
                  (Check.forAll (Check.listOf smallInt) showIntList
                     (fn ks =>
                        let val distinct = dedupKeys ks
                            val m = buildFromPairs (List.map (fn k => (k, k)) distinct)
                        in M.size m = List.length distinct end)) of
               Check.Passed _ => true
             | Check.Failed _ => false)

      (* re-inserting the same key with a different value overwrites in
         place: the size doesn't grow and the new value wins. *)
      val () =
        Harness.check "prop: insert on an existing key overwrites, not duplicates"
          (case Check.quickCheck
                  (Check.forAll
                     (Check.tuple2 (genList, Check.tuple3 (smallInt, smallInt, smallInt)))
                     (fn (xs, (k, v1, v2)) =>
                        showPairList xs ^ " k=" ^ Int.toString k
                        ^ " v1=" ^ Int.toString v1 ^ " v2=" ^ Int.toString v2)
                     (fn (xs, (k, v1, v2)) =>
                        let val m = buildFromPairs xs
                            val m1 = M.insert m k v1
                            val m2' = M.insert m1 k v2
                        in M.find m2' k = SOME v2
                           andalso M.size m2' = M.size m1
                        end)) of
               Check.Passed _ => true
             | Check.Failed _ => false)

      (* delete then reinsert round-trips to the new value. *)
      val () =
        Harness.check "prop: delete-then-reinsert round-trips"
          (case Check.quickCheck
                  (Check.forAll
                     (Check.tuple3 (genList, smallInt, smallInt))
                     (fn (xs, k, v) => showPairList xs ^ " k=" ^ Int.toString k
                                        ^ " v=" ^ Int.toString v)
                     (fn (xs, k, v) =>
                        let val m = buildFromPairs xs
                            val m1 = M.delete m k
                            val m2' = M.insert m1 k v
                        in M.find m2' k = SOME v end)) of
               Check.Passed _ => true
             | Check.Failed _ => false)
    in
      Harness.run ()
    end
end
