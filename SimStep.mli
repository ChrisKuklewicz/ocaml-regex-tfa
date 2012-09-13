(* val us : CamomileLibrary.UPervasives.uchar -> string *)

module HistoryID :
  sig
    type t = Common.patIndex * int array
    val t_of_sexp__ : Sexplib.Sexp.t -> Common.patIndex * int array
    val t_of_sexp : Sexplib.Sexp.t -> t
    val sexp_of_t : t -> Sexplib.Sexp.t
    type sexpable = t
    val compare : 'a -> 'a -> int
  end
module HistMap :
  sig
    module Key :
      sig
        type t = HistoryID.t
        val sexp_of_t : t -> Sexplib.Sexp.t
        val t_of_sexp : Sexplib.Sexp.t -> t
        val compare : t -> t -> int
        type comparator = Core.Core_map.Make(HistoryID).Key.comparator
        val comparator : (t, comparator) Core.Comparator.t_
      end
    type 'v t = (Key.t, 'v, Key.comparator) Core.Core_map.t
    val sexp_of_t : ('v -> Sexplib.Sexp.t) -> 'v t -> Sexplib.Sexp.t
    val t_of_sexp : (Sexplib.Sexp.t -> 'v) -> Sexplib.Sexp.t -> 'v t
    type ('k, 'v, 'comparator) t_ = 'v t
    type 'a key_ = Key.t
    type ('a, 'b, 'c) create_options =
        ('a, 'b, 'c) Core.Core_map_intf.create_options_without_comparator
    val empty : ('k, 'comparator, ('k, 'a, 'comparator) t_) create_options
    val singleton :
      ('k, 'comparator, 'k key_ -> 'v -> ('k, 'v, 'comparator) t_)
      create_options
    val of_alist :
      ('k, 'comparator,
       ('k key_ * 'v) list ->
       [ `Duplicate_key of 'k key_ | `Ok of ('k, 'v, 'comparator) t_ ])
      create_options
    val of_alist_exn :
      ('k, 'comparator, ('k key_ * 'v) list -> ('k, 'v, 'comparator) t_)
      create_options
    val of_alist_multi :
      ('k, 'comparator, ('k key_ * 'v) list -> ('k, 'v list, 'comparator) t_)
      create_options
    val of_alist_fold :
      ('k, 'comparator,
       ('k key_ * 'v1) list ->
       init:'v2 -> f:('v2 -> 'v1 -> 'v2) -> ('k, 'v2, 'comparator) t_)
      create_options
    val of_tree :
      ('k, 'comparator,
       ('k key_, 'v, 'comparator) Core.Core_map.tree ->
       ('k, 'v, 'comparator) t_)
      create_options
    val is_empty : ('a, 'b, 'c) t_ -> bool
    val length : ('a, 'b, 'c) t_ -> int
    val add :
      ('k, 'v, 'comparator) t_ ->
      key:'k key_ -> data:'v -> ('k, 'v, 'comparator) t_
    val add_multi :
      ('k, 'v list, 'comparator) t_ ->
      key:'k key_ -> data:'v -> ('k, 'v list, 'comparator) t_
    val change :
      ('k, 'v, 'comparator) t_ ->
      'k key_ -> ('v option -> 'v option) -> ('k, 'v, 'comparator) t_
    val find : ('k, 'v, 'a) t_ -> 'k key_ -> 'v option
    val find_exn : ('k, 'v, 'a) t_ -> 'k key_ -> 'v
    val remove :
      ('k, 'v, 'comparator) t_ -> 'k key_ -> ('k, 'v, 'comparator) t_
    val mem : ('k, 'a, 'b) t_ -> 'k key_ -> bool
    val iter : ('k, 'v, 'a) t_ -> f:(key:'k key_ -> data:'v -> unit) -> unit
    val map :
      ('k, 'v1, 'comparator) t_ ->
      f:('v1 -> 'v2) -> ('k, 'v2, 'comparator) t_
    val mapi :
      ('k, 'v1, 'comparator) t_ ->
      f:(key:'k key_ -> data:'v1 -> 'v2) -> ('k, 'v2, 'comparator) t_
    val fold :
      ('k, 'v, 'b) t_ ->
      init:'a -> f:(key:'k key_ -> data:'v -> 'a -> 'a) -> 'a
    val fold_right :
      ('k, 'v, 'b) t_ ->
      init:'a -> f:(key:'k key_ -> data:'v -> 'a -> 'a) -> 'a
    val filter :
      ('k, 'v, 'comparator) t_ ->
      f:(key:'k key_ -> data:'v -> bool) -> ('k, 'v, 'comparator) t_
    val filter_map :
      ('k, 'v1, 'comparator) t_ ->
      f:('v1 -> 'v2 option) -> ('k, 'v2, 'comparator) t_
    val filter_mapi :
      ('k, 'v1, 'comparator) t_ ->
      f:(key:'k key_ -> data:'v1 -> 'v2 option) -> ('k, 'v2, 'comparator) t_
    val compare :
      ('v -> 'v -> int) ->
      ('k, 'v, 'comparator) t_ -> ('k, 'v, 'comparator) t_ -> int
    val equal :
      ('v -> 'v -> bool) ->
      ('k, 'v, 'comparator) t_ -> ('k, 'v, 'comparator) t_ -> bool
    val keys : ('k, 'a, 'b) t_ -> 'k key_ list
    val data : ('a, 'v, 'b) t_ -> 'v list
    val to_alist : ('k, 'v, 'a) t_ -> ('k key_ * 'v) list
    val merge :
      ('k, 'v1, 'comparator) t_ ->
      ('k, 'v2, 'comparator) t_ ->
      f:(key:'k key_ ->
         [ `Both of 'v1 * 'v2 | `Left of 'v1 | `Right of 'v2 ] -> 'v3 option) ->
      ('k, 'v3, 'comparator) t_
    val min_elt : ('k, 'v, 'a) t_ -> ('k key_ * 'v) option
    val min_elt_exn : ('k, 'v, 'a) t_ -> 'k key_ * 'v
    val max_elt : ('k, 'v, 'a) t_ -> ('k key_ * 'v) option
    val max_elt_exn : ('k, 'v, 'a) t_ -> 'k key_ * 'v
    val for_all : ('k, 'v, 'a) t_ -> f:('v -> bool) -> bool
    val exists : ('k, 'v, 'a) t_ -> f:('v -> bool) -> bool
    val fold_range_inclusive :
      ('k, 'v, 'b) t_ ->
      min:'k key_ ->
      max:'k key_ -> init:'a -> f:(key:'k key_ -> data:'v -> 'a -> 'a) -> 'a
    val range_to_alist :
      ('k, 'v, 'a) t_ -> min:'k key_ -> max:'k key_ -> ('k key_ * 'v) list
    val prev_key : ('k, 'v, 'a) t_ -> 'k key_ -> ('k key_ * 'v) option
    val next_key : ('k, 'v, 'a) t_ -> 'k key_ -> ('k key_ * 'v) option
    val rank : ('k, 'a, 'b) t_ -> 'k key_ -> int option
    val to_tree :
      ('k, 'v, 'comparator) t_ ->
      ('k key_, 'v, 'comparator) Core.Core_map.tree
  end

type postData = {
  pHistory : Common.history;
  pPostTag : Common.tag option;
  pContext : (Simulate.simStack * CorePattern.coreQ) list;
}

type histMap = postData HistMap.t

type stepData =
    StepChar of (Common.strIndex * ReadPattern.uchar)
  | StepEnd of Common.strIndex

type simFeed = stepData -> Common.history list

val simStep :
  ?prevIn:Common.strIndex * ReadPattern.uchar ->
  CorePattern.coreResult -> simFeed

type o = (Common.groupCap * Common.history) list

val uWrap : CorePattern.coreResult -> ReadPattern.ustring -> o

val wrapSimStep : ReadPattern.ustring -> ReadPattern.ustring -> o

val kick :
  ReadPattern.ustring -> ReadPattern.ustring Core.Core_list.t -> unit

val test : unit -> unit
