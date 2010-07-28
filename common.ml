(* Common.ml *)

TYPE_CONV_PATH "Common"

type patIndex = int with sexp
type groupIndex = int with sexp
type strIndex = int with sexp

(* Tags are indices into an array of input positions or orbit data.  When the tag is triggered the
   related task is activated.  This can be done before or after updating the input position when a
   transition is being made. *)
type tag = int with sexp
type rep = int with sexp

type tagTask = TagTask (* Maximize or Minimize *)
               | ResetGroupStopTask (* GroupFlag *)
               | SetGroupStopTask   (* GroupFlag *)
               | EnterOrbitTask  (* On entry to repeat *)
               | LoopOrbitTask   (* Loop from end to start of repeat *)
               | ResetOrbitTask  (* After entry or loop entry to repeat *)
               | LeaveOrbitTask  (* On leaving repeat *)
with sexp
type repTask = IncRep of int | LeaveRep with sexp
type tagTasks = (tag*tagTask) list * (rep*repTask) list with sexp
(* type 'a taskUpdate = PreUpdate of 'a | PostUpdate of 'a with sexp *)
type taskList = (tag*tagTask) list * (rep*repTask) list with sexp
type tagOP = Maximize | Minimize | Orbit | GroupFlag with sexp
8
(* When building the NFA a given node in the regexp tree may want to lead to an NFA state or a
   bundle of transitions or the node may not have a preference

   By precomputing these preferences the NFA builder can avoid creating NFA states that will not be
   part of the final NFA.
*)
type wanted = WantsState | WantsBundle | WantsEither with sexp

type tagAction = SetPre | SetPost | SetVal of int

type orbitEntry = { inOrbit : bool
                  ; basePos : strIndex
                  ; ordinalVal : int option
                  ; getOrbits : strIndex list
                  }

module IntMap = Core.Core_map.Make(Core.Core_int)
type orbitLog = orbitEntry IntMap.t

type orbitTransformer = orbitLog -> orbitLog

