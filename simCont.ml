(* SimCont.ml *)

(*

  A modfied simStep.ml to operate without the context stack.  This requires adding continuation information to the tree.

  State between accepting characters is still logically stored in OneChar locations.

*)

open Sexplib.Std
open CamomileLibrary
open Common
open WhichTest
open Pattern
open ReadPattern
open CorePattern
open Simulate
open SimStep
open Core.Result

TYPE_CONV_PATH "SimCont"

