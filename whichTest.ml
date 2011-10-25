(* WhichTest.ml *)

(* This module defines the operations on "tests" such as the anchors ^ and $ which test beginning
   and ending of a line or string.  To be extensible this module understands that more tests can be
   added and that any test can be negated.

   This module also provides routines to allow primitive tests to be combined into "testSet"s that
   all have to be satisfied simultaneously.  These sets have a monoidal structure which is
   implemented here.

   Another operation is to check whether testSet y being fully satisfied implies that testSet x is
   fully satisfied.  Conversely if testSet x is not satisfied than testSet y cannot be satisfied,
   and thus "dominates x y" is true. In this case if testSet x will be checked before and in
   preference to testSet y then the program can discard the possibility of testSet y being checked.
   This implication structure means that the testSets form a partial order.

   The whichTestMap in CheckAll must never be an empty map.  Instead of an "empty map" the
   AlwaysTrue must be used.
*)

open Sexplib.Std
open Common
open Monoid

TYPE_CONV_PATH "WhichTest"

(* There are only two assertion tests in extended POSIX regular expressions.  More can be added
   later by extended this whichTest enumeration, adding parsing capability, and adding testing code
   to the engines. *)
type whichTest = 
    Test_BOL 
  | Test_EOL
with sexp

module WhichTestCompare = struct
  type t = whichTest
  type sexpable = t
  let sexp_of_t u = sexp_of_whichTest u
  let t_of_sexp s = whichTest_of_sexp s
  let compare = Pervasives.compare
end

module WhichTestMap = Core.Core_map.Make(WhichTestCompare)

type whichTestMap = (bool*(patIndex list)) WhichTestMap.t
with sexp

(* Assert side condition that whichTestMap is never empty *)
type testSet = AlwaysTrue | AlwaysFalse | CheckAll of whichTestMap
with sexp

(* convenience method *)
let singleTest (b,wt,i) = CheckAll (WhichTestMap.singleton wt (b,[i]))

(* nullView is an important piece of corePattern/coreQ that determines how zero character qaccepting
   possibilies are handled *)
type nullView = (testSet*taskList) list with sexp

(* SetFalse is used in the WhichTestMonoid below to short circuit some testing *)
exception SetFalse

module WhichTestMonoid =
  (struct
    type t = testSet
    let mempty = AlwaysTrue
    let mappend x y = match (x,y) with
        (AlwaysTrue,_) -> y
      | (_,AlwaysTrue) -> x
      | (AlwaysFalse,_) -> AlwaysFalse
      | (_,AlwaysFalse) -> AlwaysFalse
      | (CheckAll a,CheckAll b) -> 
        let mergeWTM ~key optA optB =
          match (optA,optB) with
              (_,None) -> optA
            | (None,_) -> optB
            | (Some (tf1,a),Some (tf2,b)) when tf1=tf2 -> Some (tf1,a @ b)
            | _ -> raise SetFalse (* no need to test the rest *)
        in
        try CheckAll (WhichTestMap.merge mergeWTM a b)
        with SetFalse -> AlwaysFalse
    let mconcat = function xs -> List.fold_left mappend mempty xs
   end)

(* This is true if x dominates y and thus y can be ignored *)
let dominates x y = match (x,y) with
    (AlwaysTrue,_) -> true
  | (AlwaysFalse,_) -> false
  | (_,AlwaysFalse) -> true
  | (_,AlwaysTrue) -> false
  | (CheckAll a,CheckAll b) ->
    let test ~key ~data:(b1,_) =
      match WhichTestMap.find b key with
          Some (b2,_) when b1=b2 -> ()
        | _ -> raise SetFalse
    in
    try (WhichTestMap.iter test a ; true)
    with SetFalse -> false

