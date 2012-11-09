(*pp camlp4o -I `ocamlfind query type_conv` -I `ocamlfind query sexplib` pa_type_conv.cma pa_sexp_conv.cma *)
(* Common.ml *)

(* changed type-conv to type_conv *)

open Sexplib.Std

TYPE_CONV_PATH "Common"

(* Index into string form of regular expression *)
type patIndex = int with sexp
(* Index into string being searched, stored in history.tagA and history.orbitA *)
type strIndex = int with sexp

(* Index of capture group, 0 is always the whole pattern, into array groupCap *)
type groupIndex = int with sexp
(* Index specifying a specific 'tag' into an array of input positions: history.tagA.  *)
type tag = int with sexp
(* Index specifying a specific 'repeat' into array of orbit data: history.orbitA *)
type orbit = int with sexp
(* Index of a specific 'repeat' into array of count of repetitions: history.repA *)
type rep = int with sexp

type tagTask = TagTask (* Maximize or Minimize *)
               | ResetGroupStopTask (* GroupFlag *)
               | SetGroupStopTask   (* GroupFlag *)
with sexp

type orbitTask = EnterOrbitTask  (* On entry to repeat *)
                 | LoopOrbitTask   (* Loop from end to start of repeat *)
                 | ResetOrbitTask  (* After entry or loop entry to repeat *)
                 | LeaveOrbitTask  (* On leaving repeat *)
with sexp

type repTask = IncRep of int (* int is highest allow count in repA *)
               | LeaveRep 
with sexp

(* tagTask and repTask items commute: make them separate lists *)
type taskList = { tlTag : (tag*tagTask) list
                ; tlOrbit : (rep*orbit*orbitTask) list
                ; tlRep : (rep*repTask) list }
with sexp

let emptyTaskList = { tlTag = []; tlOrbit = []; tlRep = [] };

(* The int with Orbit is an array index *)
type tagOP = Maximize | Minimize | Orbit of orbit | GroupFlag with sexp

(* comment out unused things from Haskell *)
(* type 'a taskUpdate = PreUpdate of 'a | PostUpdate of 'a with sexp *)
(* type tagAction = SetPre | SetPost | SetVal of int *)

(* When building the NFA a given node in the regexp tree may want to lead to an NFA state or a
   bundle of transitions or the node may not have a preference

   By precomputing these preferences the NFA builder can avoid creating NFA states that will not be
   part of the final NFA.
*)
type wanted = WantsState | WantsBundle | WantsEither with sexp

(* orbitEntry, orbitLog, orbitTransformer are currently unused *)
(*
type orbitEntry = { inOrbit : bool             (* *)
                  ; basePos : strIndex         (* *)
                  ; ordinalVal : int option    (* for efficiency via sorting pass *)
                  ; getOrbits : strIndex list  (* *)
                  }

module IntMap = Core.Core_map.Make(Core.Core_int)
type orbitLog = orbitEntry IntMap.t

type orbitTransformer = orbitLog -> orbitLog
*)

(* convenience names for iterating *)
let forList list f = Core.Core_list.iter list ~f:f
let forOpt opt f = Core.Option.iter opt ~f:f
let forArray arr f = Core.Core_array.iter arr ~f:f
let mapOpt f o = Core.Option.map o ~f:f
let forIArray arr f = Core.Core_array.iteri arr ~f:f

type groupCap = (strIndex*strIndex) array
with sexp
