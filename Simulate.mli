val ignore : 'a -> unit
type notice = NoNote | NoteNoLoop
type simStack = SimReturn of notice | SimEnterAny | SimEnterAccept
val seeList : int list -> string
val newline : CamomileLibrary.UChar.t
val stringToList :
  ReadPattern.ustring -> (Common.strIndex * ReadPattern.uchar) list
val comparePos : 'a list -> 'a list -> int
val compareHistory :
  Common.tagOP array -> Common.history -> Common.history -> int
val interpretGroups :
  int -> CorePattern.groupInfo array -> Common.history -> Common.groupCap
val doTagTask : int -> Common.history -> int * Common.tagTask -> unit
val doOrbitTask :
  int -> Common.history -> int * int * Common.orbitTask -> unit
val doRepTask : Common.history -> int * Common.repTask -> unit
val doTasks : int -> Common.history -> Common.taskList -> Common.history
val simCP :
  ?prevIn:CamomileLibrary.UTF8.index * ReadPattern.uchar * int ->
  CorePattern.coreResult ->
  ReadPattern.ustring -> (Common.groupCap * Common.history) list
val seeSimResult : Common.groupCap * Common.history -> unit
val kick :
  ReadPattern.ustring -> ReadPattern.ustring Core.Core_list.t -> unit
val test : unit -> unit
val test2 : unit -> unit
