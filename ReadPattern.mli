module UTF8Set :
  sig
    type elt = CamomileLibrary.UTF8.t
    type t = Set.Make(CamomileLibrary.UTF8).t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
  end

type utf8Set = UTF8Set.t
type uchar = CamomileLibrary.UChar.t
type ustring = CamomileLibrary.UTF8.t
type parseEnd = (Pattern.pattern, string) Core.Result.t
type parseF =
    ParseWith of (int * uchar -> parseF) * (unit -> parseEnd)
  | ParseError of string

val see :
  (Pattern.pattern, CamomileLibrary.UTF8.t) Core.Result.t ->
  CamomileLibrary.UTF8.t

(*
val u : char -> CamomileLibrary.UChar.t
val e : CamomileLibrary.UChar.t -> CamomileLibrary.UTF8.t
val ee : CamomileLibrary.UChar.t list -> CamomileLibrary.UTF8.t
val pr : ('a, out_channel, unit) format -> 'a
val spr : ('a, unit, string) format -> 'a
*)

val iterParse : parseF -> ustring -> parseEnd

val toUSet : char list -> CamomileLibrary.USet.t

val specials : CamomileLibrary.USet.t

val digits : CamomileLibrary.USet.t

val allowedClasses : UTF8Set.t

val unexpected : CamomileLibrary.UChar.t -> parseF

type parseEnv = {
  parentGroupIndex : Common.groupIndex;
  nextGroupIndexRef : Common.groupIndex ref;
  popCont : (Pattern.pattern -> parseF) option;
}

val pushBranch :
  parseEnv -> Pattern.branchPat list -> Common.patIndex * uchar -> parseF

val initialParser : unit -> parseF

val parseRegex : ustring -> parseEnd

(*
val ick : ustring -> parseEnd
*)
