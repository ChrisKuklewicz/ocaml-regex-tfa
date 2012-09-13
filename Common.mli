val forList : 'a Core.Core_list.t -> f:('a -> unit) -> unit
val forOpt : 'a Core.Option.t -> f:('a -> unit) -> unit
val forArray : 'a Core.Core_array.t -> f:('a -> unit) -> unit
val forIArray : 'a Core.Core_array.t -> (int -> 'a -> unit) -> unit

type patIndex = int
val patIndex_of_sexp__ : Sexplib.Sexp.t -> int
val patIndex_of_sexp : Sexplib.Sexp.t -> patIndex
val sexp_of_patIndex : patIndex -> Sexplib.Sexp.t

type groupIndex = int
val groupIndex_of_sexp__ : Sexplib.Sexp.t -> int
val groupIndex_of_sexp : Sexplib.Sexp.t -> groupIndex
val sexp_of_groupIndex : groupIndex -> Sexplib.Sexp.t

type strIndex = int
val strIndex_of_sexp__ : Sexplib.Sexp.t -> int
val strIndex_of_sexp : Sexplib.Sexp.t -> strIndex
val sexp_of_strIndex : strIndex -> Sexplib.Sexp.t

type tag = int
val tag_of_sexp__ : Sexplib.Sexp.t -> int
val tag_of_sexp : Sexplib.Sexp.t -> tag
val sexp_of_tag : tag -> Sexplib.Sexp.t

type orbit = int
val orbit_of_sexp__ : Sexplib.Sexp.t -> int
val orbit_of_sexp : Sexplib.Sexp.t -> orbit
val sexp_of_orbit : orbit -> Sexplib.Sexp.t

type rep = int
val rep_of_sexp__ : Sexplib.Sexp.t -> int
val rep_of_sexp : Sexplib.Sexp.t -> rep
val sexp_of_rep : rep -> Sexplib.Sexp.t

type tagTask = TagTask | ResetGroupStopTask | SetGroupStopTask
val tagTask_of_sexp__ : Sexplib.Sexp.t -> tagTask
val tagTask_of_sexp : Sexplib.Sexp.t -> tagTask
val sexp_of_tagTask : tagTask -> Sexplib.Sexp.t

type orbitTask =
    EnterOrbitTask
  | LoopOrbitTask
  | ResetOrbitTask
  | LeaveOrbitTask
val orbitTask_of_sexp__ : Sexplib.Sexp.t -> orbitTask
val orbitTask_of_sexp : Sexplib.Sexp.t -> orbitTask
val sexp_of_orbitTask : orbitTask -> Sexplib.Sexp.t

type repTask = IncRep of int | LeaveRep
val repTask_of_sexp__ : Sexplib.Sexp.t -> repTask
val repTask_of_sexp : Sexplib.Sexp.t -> repTask
val sexp_of_repTask : repTask -> Sexplib.Sexp.t

type taskList = {
  tlTag : (tag * tagTask) list;
  tlOrbit : (rep * orbit * orbitTask) list;
  tlRep : (rep * repTask) list;
}
val taskList_of_sexp__ : Sexplib.Sexp.t -> taskList
val taskList_of_sexp : Sexplib.Sexp.t -> taskList
val sexp_of_taskList : taskList -> Sexplib.Sexp.t

val emptyTaskList : taskList

type tagOP = Maximize | Minimize | Orbit of int | GroupFlag
val tagOP_of_sexp__ : Sexplib.Sexp.t -> tagOP
val tagOP_of_sexp : Sexplib.Sexp.t -> tagOP
val sexp_of_tagOP : tagOP -> Sexplib.Sexp.t

type wanted = WantsState | WantsBundle | WantsEither
val wanted_of_sexp__ : Sexplib.Sexp.t -> wanted
val wanted_of_sexp : Sexplib.Sexp.t -> wanted
val sexp_of_wanted : wanted -> Sexplib.Sexp.t

type history = {
  tagA : int array;
  repA : int array;
  orbitA : int list array;
}
val history_of_sexp__ : Sexplib.Sexp.t -> history
val history_of_sexp : Sexplib.Sexp.t -> history
val sexp_of_history : history -> Sexplib.Sexp.t

type groupCap = (int * int) array
val groupCap_of_sexp__ : Sexplib.Sexp.t -> (int * int) array
val groupCap_of_sexp : Sexplib.Sexp.t -> groupCap
val sexp_of_groupCap : groupCap -> Sexplib.Sexp.t

val copyHistory : history -> history

val safeHistory : history -> unit -> history

type 'a continueTo = ContEnter of 'a | ContReturn of 'a | ContRoot
val continueTo_of_sexp__ :
  (Sexplib.Sexp.t -> 'a) -> Sexplib.Sexp.t -> 'a continueTo
val continueTo_of_sexp :
  (Sexplib.Sexp.t -> 'a) -> Sexplib.Sexp.t -> 'a continueTo
val sexp_of_continueTo :
  ('a -> Sexplib.Sexp.t) -> 'a continueTo -> Sexplib.Sexp.t
