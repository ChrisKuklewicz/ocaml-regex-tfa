(*pp camlp4o -I `ocamlfind query type_conv` -I `ocamlfind query sexplib` pa_type_conv.cma pa_sexp_conv.cma *)
(* Common.ml *)

(* changed type-conv to type_conv *)

open Sexplib.Std

TYPE_CONV_PATH "Common"

type patIndex = int with sexp
type groupIndex = int with sexp
type strIndex = int with sexp

(* Tags are indices into an array of input positions or orbit data.  When the tag is triggered the
   related task is activated.  This can be done before or after updating the input position when a
   transition is being made. *)
type tag = int with sexp
type orbit = int with sexp
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

type repTask = IncRep of int 
               | LeaveRep 
with sexp

(* tagTask and repTask items commute: make them separate lists *)
type taskList = { tlTag : (tag*tagTask) list
                ; tlOrbit : (rep*orbit*orbitTask) list
                ; tlRep : (rep*repTask) list }
with sexp

let emptyTaskList = { tlTag = []; tlOrbit = []; tlRep = [] };

(* The int with Orbit is an array index *)
type tagOP = Maximize | Minimize | Orbit of int | GroupFlag with sexp

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
let forList = Core.Core_list.iter
let forOpt = Core.Option.iter
let forArray = Core.Core_array.iter
(* let mapOpt f o = Core.Option.map o ~f:f *)

let forIArray f arr = Core.Core_array.iteri arr f

(* The history of a given match possibility *)
type history = { tagA : int array
               ; repA : int array
               ; orbitA : (int list) array (* inefficient *)
               }
with sexp

type groupCap = (int*int) array
with sexp

let copyHistory { tagA=a;repA=b;orbitA=c } = { tagA = Array.copy a
                                             ; repA = Array.copy b
                                             ; orbitA = Array.copy c
                                             }

let safeHistory h = fun () -> copyHistory h

(* Used in saveContext as a concrete call stack *)
type 'a continueTo = ContEnter of 'a | ContReturn of 'a | ContRoot
with sexp
