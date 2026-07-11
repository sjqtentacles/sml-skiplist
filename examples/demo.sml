(* demo.sml - a persistent skip-list map keyed by int, showing insert, find,
   min/max, ordered traversal, size, and delete. Deterministic: the functor's
   embedded PRNG is seeded internally, so level draws never vary. *)

structure M = SkipList (struct type t = int val compare = Int.compare end)

fun showList m =
  print ("  toList = ["
         ^ String.concatWith ", "
             (List.map (fn (k, v) => "(" ^ Int.toString k ^ "," ^ Int.toString v ^ ")")
                       (M.toList m))
         ^ "]\n")

fun showOpt NONE = "NONE"
  | showOpt (SOME (k, v)) = "SOME (" ^ Int.toString k ^ "," ^ Int.toString v ^ ")"

val keys = [5, 2, 8, 1, 9, 3]
val m = List.foldl (fn (k, acc) => M.insert acc k (k * 3 + 1)) M.empty keys

val () = print "Inserted keys [5,2,8,1,9,3] with value = key*3+1:\n"
val () = showList m
val () = print ("  size = " ^ Int.toString (M.size m) ^ "\n")
val () = print ("  find 8 = " ^ (case M.find m 8 of NONE => "NONE" | SOME v => "SOME " ^ Int.toString v) ^ "\n")
val () = print ("  find 4 = " ^ (case M.find m 4 of NONE => "NONE" | SOME v => "SOME " ^ Int.toString v) ^ "\n")
val () = print ("  min = " ^ showOpt (M.min m) ^ "\n")
val () = print ("  max = " ^ showOpt (M.max m) ^ "\n")

val m2 = M.delete m 8
val () = print "\nAfter delete 8:\n"
val () = showList m2
val () = print ("  size = " ^ Int.toString (M.size m2) ^ "\n")
