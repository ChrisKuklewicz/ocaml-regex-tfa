(* SimCont.ml *)

(*

  A modfied simCont.ml to operate in passes over the tree instead of iterating over history.  Changes scaling of worst case time.

  State between accepting characters is still logically stored in OneChar locations.  A

*)

open Sexplib.Std
open Sexplib.Sexp
open Sexplib
open CamomileLibrary
open Common
open WhichTest
open Pattern
open ReadPattern
open CorePattern
open Simulate
open SimStep
open Core.Result

TYPE_CONV_PATH "SimFlush"

(* Setup storage types for matching data *)

module PatIndexID =
struct
  type t = patIndex with sexp
  type sexpable = t
  let compare = compare
end

module RepStateID =
struct
  type t = int array with sexp
  type sexpable = t
  let compare = compare
end

module BundleMap = Core.Core_map.Make(RepStateID)

module IndexMap = Core.Core_map.Make(PatIndexID)

type bundle = history BundleMap.t

type indexMap = bundle IndexMap.t

let forBundle b g = BundleMap.iter (fun ~key:_ ~data:h -> g h) b


let copyBundle b = b (* XXX *)

let todo = failwith "todo"

let simCont ?(prevIn=(-1,newline)) (cr : coreResult) : simFeed =
  let numTags = Array.length cr.tags
  and numReps = cr.depthCount
  and root = cr.cp
  in
  let prev = ref prevIn  (* Anchors can test facts about preceding character *)
  and winners = ref []
  and m1 = ref IndexMap.empty  (* on each iteration vivify possibilities from here *)
  and m2 = ref IndexMap.empty  (* while matching store paused possibilities here *)
  and startBundle = ref BundleMap.empty
  and startHistory = { tagA   = Array.make numTags (-1)
                     ; repA   = Array.make numReps 0
                     ; orbitA = Array.make numTags []
                     }
  in
  let cycle here =
    prev := here;
    m1 := !m2;
    m2 := IndexMap.empty;
    let listWinners = !winners in
    winners := [];
    listWinners
  in
  
  let bestBundle in1 in2 =
    match (in1,in2) with
        (Some b1, Some b2) -> Some (
          BundleMap.merge (fun ~key:_ opt1 opt2 -> 
            match (opt1,opt2) with
                (Some h1, Some h2) ->
                  begin
                    match compareHistory cr.tags h1 h2 with
                        1 -> Some h2
                      | _ -> Some h1
                  end
              | (Some h1, None) -> Some h1
              | (None, Some h2) -> Some h2
              | (None,None) -> None) b1 b2)
      | (Some b1,None) -> Some b1
      | (None, Some b2) -> Some b2
      | (None, None) -> None
  in

  let (mergeBundles : bundle option list -> bundle option) = function
    | [] -> None
    | (b::bs) -> List.fold_left bestBundle b bs
  in

  let rec nextStep = function
    | StepChar ((i,_c) as here) ->
      flushUp here root

    | StepEnd indexAtEnd ->
      todo

  and dispatchFlush here b at =
    match at.contTo with
        HowReturn q -> flushUp
      | HowReturnMidSeq q -> todo
      | HowRoot -> todo

  and flushUp ((i,_c) as here) q : bundle option =
    let doPostTag : bundle option -> bundle option = function
      | None -> None
      | Some bContinue -> 
        forOpt q.postTag (fun tag ->
          forBundle bContinue (fun hContinue ->
            doTagTask i hContinue (tag,TagTask)));
        Some bContinue
    in
    match q.unQ with
        OneChar (_uc,patIndex) ->
          begin
            match IndexMap.find !m2 patIndex with
                None -> None
              | Some b -> doPostTag (Some (copyBundle b))
          end
      | Seq (qFront,qBack) ->
        let bFront = flushUp here qFront
        in
        let bBack1 = doEnterNull here bFront qBack
        and bBack2 = flushUp here qBack
        in
        doPostTag (mergeBundles [bBack1; bBack2])

      | Test _ -> todo
      | Repeat _ -> todo
      | Or _ -> todo
      | CaptureGroup _ -> todo

  and doEnterNull = todo

  in todo
