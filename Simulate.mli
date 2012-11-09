val ignore : 'a -> unit
type notice = NoNote | NoteNoLoop
type simStack = SimReturn of notice | SimEnterAny | SimEnterAccept
val seeList : int list -> string
val newline : CamomileLibrary.UChar.t
val stringToList :
  ReadPattern.ustring -> (Common.strIndex * ReadPattern.uchar) list
val comparePos : 'a list -> 'a list -> int
val compareHistory :
  Common.tagOP array -> History.history -> History.history -> int
val interpretGroups :
  Common.strIndex ->
  CorePattern.groupInfo array -> 'o History.historyP -> Common.groupCap
val doTagTask :
  Common.strIndex ->
  'o History.historyP -> Common.tag * Common.tagTask -> unit
val doOrbitTask :
  Common.strIndex ->
  History.history -> Common.tag * Common.orbit * Common.orbitTask -> unit
val doRepTask : 'o History.historyP -> int * Common.repTask -> unit
val doTasks :
  Common.strIndex -> History.history -> Common.taskList -> History.history
val simCP :
  ?prevIn:CamomileLibrary.UTF8.index * ReadPattern.uchar * Common.strIndex ->
  CorePattern.coreResult ->
  ReadPattern.ustring -> (Common.groupCap * History.history) list
val seeSimResult : Common.groupCap * History.history -> unit
val kick :
  ReadPattern.ustring -> ReadPattern.ustring Core.Core_list.t -> unit
val test : unit -> unit
val test2 : unit -> unit
