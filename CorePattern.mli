(* val ignore : 'a -> unit *)

type uset = CamomileLibrary.USet.t
val sexp_of_uset : CamomileLibrary.USet.t -> Sexplib.Sexp.t
val uset_of_sexp : Sexplib.Sexp.t -> CamomileLibrary.USet.t

val all_unicode : CamomileLibrary.USet.t

(* val take_append : int -> 'a list -> 'a list -> 'a list *)

val liftOpt : ('a -> 'b -> 'c) -> 'a option -> 'b option -> 'c option

val pureRepeatOnto : 'a list -> 'a -> int -> 'a list

val thunkRepeatOnto : 'a list -> (unit -> 'a) -> int -> 'a list

type handleTag = NoTag | Advice of Common.tag | Apply of Common.tag
val handleTag_of_sexp__ : Sexplib.Sexp.t -> handleTag
val handleTag_of_sexp : Sexplib.Sexp.t -> handleTag
val sexp_of_handleTag : handleTag -> Sexplib.Sexp.t

val seeht : handleTag -> string

val apply : handleTag -> Common.tag option

val asAdvice : handleTag -> handleTag

val toUpdate : handleTag -> (Common.tag * Common.tagTask) list

type corePattern =
    Or of coreQ list
  | Seq of coreQ * coreQ
  | Repeat of repeatQ
  | Test of WhichTest.testSet
  | OneChar of uset * Common.patIndex
  | CaptureGroup of capGroupQ
and capGroupQ = {
  parentGroup : Common.groupIndex;
  myGroup : Common.groupIndex;
  mutable preReset : Common.tag list;
  mutable postSet : Common.tag;
  subPat : coreQ;
}
and repeatQ = {
  lowBound : int;
  optHiBound : int option;
  topCount : int;
  repDepth : int;
  needsOrbit : bool;
  mutable getOrbit : (Common.tag * Common.orbit) option;
  mutable resetOrbits : (Common.tag * Common.orbit) list;
  mutable mostLoops : int;
  repAt : Common.patIndex;
  unRep : coreQ;
}
and contSpec = HowReturn of coreQ | HowReturnMidSeq of coreQ | HowRoot
and coreQ = {
  takes : int * int option;
  childGroups : bool;
  tagged : bool;
  wants : Common.wanted;
  mutable preTag : Common.tag option;
  mutable postTag : Common.tag option;
  mutable nullQ : WhichTest.nullView;
  mutable contTo : contSpec;
  unQ : corePattern;
}

val corePattern_of_sexp__ : Sexplib.Sexp.t -> corePattern
val corePattern_of_sexp : Sexplib.Sexp.t -> corePattern
val capGroupQ_of_sexp__ : Sexplib.Sexp.t -> capGroupQ
val capGroupQ_of_sexp : Sexplib.Sexp.t -> capGroupQ
val repeatQ_of_sexp__ : Sexplib.Sexp.t -> repeatQ
val repeatQ_of_sexp : Sexplib.Sexp.t -> repeatQ
val contSpec_of_sexp__ : Sexplib.Sexp.t -> contSpec
val contSpec_of_sexp : Sexplib.Sexp.t -> contSpec
val coreQ_of_sexp__ : Sexplib.Sexp.t -> coreQ
val coreQ_of_sexp : Sexplib.Sexp.t -> coreQ
val sexp_of_corePattern : corePattern -> Sexplib.Sexp.t
val sexp_of_capGroupQ : capGroupQ -> Sexplib.Sexp.t
val sexp_of_repeatQ : repeatQ -> Sexplib.Sexp.t
val sexp_of_contSpec : contSpec -> Sexplib.Sexp.t
val sexp_of_coreQ : coreQ -> Sexplib.Sexp.t

val nothing : coreQ

val epsilon : coreQ

type groupInfo = {
  parentIndex : Common.groupIndex;
  thisIndex : Common.groupIndex;
  startTag : Common.tag;
  stopTag : Common.tag;
  flagTag : Common.tag;
}
val groupInfo_of_sexp__ : Sexplib.Sexp.t -> groupInfo
val groupInfo_of_sexp : Sexplib.Sexp.t -> groupInfo
val sexp_of_groupInfo : groupInfo -> Sexplib.Sexp.t

type coreResult = {
  cp : coreQ;
  tags : Common.tagOP array;
  groups : groupInfo array;
  orbitCount : int;
  depthCount : int;
}
val coreResult_of_sexp__ : Sexplib.Sexp.t -> coreResult
val coreResult_of_sexp : Sexplib.Sexp.t -> coreResult
val sexp_of_coreResult : coreResult -> Sexplib.Sexp.t

val varies : 'a * 'a option -> bool

val toUSet : Pattern.bracketPiece list -> CamomileLibrary.USet.t

val cannotTake : coreQ -> bool

val addGroupResetsToNullView :
  Common.tag list ->
  Common.tag -> ('a * Common.taskList) list -> ('a * Common.taskList) list

val cleanNullView : WhichTest.nullView -> WhichTest.nullView

val seqNullViews :
  WhichTest.nullView -> WhichTest.nullView -> WhichTest.nullView

val tagWrapNullView :
  handleTag ->
  handleTag -> ('a * Common.taskList) list -> ('a * Common.taskList) list

val orbitWrapNullView :
  repeatQ ->
  (Common.rep * Common.orbit) option ->
  (Common.rep * Common.orbit) list ->
  ('a * Common.taskList) list -> ('a * Common.taskList) list

type appliedBoth = AppliedBoth

val toCorePattern : Pattern.pattern -> coreResult

val kick : ReadPattern.ustring -> unit

val test : unit -> unit
