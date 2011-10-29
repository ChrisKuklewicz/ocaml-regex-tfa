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
open List
open Core
(*open Core.Option
open Core.Core_list
open Core.Core_map*)
(*open Core.List*)

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

let forBundle (b : bundle) (g : history -> unit) : unit =
  BundleMap.iter (fun ~key:_ ~data:h -> g h) b

let forOptBundle optB g = 
  match optB with
      None -> ()
    | Some b -> forBundle b g

let copyBundle b = BundleMap.of_alist_exn 
  (Core.Core_list.map 
     (BundleMap.to_alist b) 
     ~f:(fun (key,h) -> (key,copyHistory h)))

let copyOptBundle b = Option.map b ~f:copyBundle

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

  let bestHistory h1 h2 =
    match compareHistory cr.tags h1 h2 with
        1 -> h2
      | _ -> h1
  in

  let bestHistoryList hs = match hs with
      [] -> failwith "bestHistoryList failed with [] parameter"
    | x::xs -> fold_left bestHistory x xs
  in

  let bestBundle' b1 b2 =
    BundleMap.merge (fun ~key:_ opt1 opt2 -> 
      match (opt1,opt2) with
          (Some h1, Some h2) -> Some (bestHistory h1 h2)
        | (Some h1, None) -> Some h1
        | (None, Some h2) -> Some h2
        | (None,None) -> None) b1 b2
  in

  let bestBundle in1 in2 =
    match (in1,in2) with
        (Some b1, Some b2) -> Some (bestBundle' b1 b2)
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
      forOptBundle (flushUp here root) (doWin i);
      forOpt (tryTaskList here root) (fun taskList ->
        let h = copyHistory startHistory in
        doTagTask i h (0,TagTask);
        doWin i (doTasks i h taskList));
      let h = copyHistory startHistory in
      doTagTask i h (0,TagTask);
      let b = BundleMap.singleton h.repA h in
      ignore (doEnter here (Some b) root)

    | StepEnd indexAtEnd ->
      todo

  and doWin i h =
    doTagTask i h (1,TagTask);
    winners := h :: !winners

  and flushUp ((i,_c) as here) q : bundle option =
    if (Some 0 = snd q.takes)
    then None
    else
      let doPostTag optB =
        forOpt optB (fun bContinue ->
          forOpt q.postTag (fun tag ->
            forBundle bContinue (fun hContinue ->
              doTagTask i hContinue (tag,TagTask))))
      and bUp = match q.unQ with
          OneChar (_uc,patIndex) -> Option.map (IndexMap.find !m2 patIndex) ~f:copyBundle

        | Seq (qFront,qBack) ->
          (* perhaps better: check tryTaskList on qBack before flushUp on bFront *)
          let bFront = flushUp here qFront
          in 
          let bBack1 = doEnterNull_bundle here bFront qBack
          and bBack2 = flushUp here qBack
          in bestBundle bBack1 bBack2
          
        | Or qs -> mergeBundles (List.map (flushUp here) qs)

        | CaptureGroup cg ->
          let bSubPat = flushUp here cg.subPat
          in 
          forOptBundle bSubPat (fun h ->
            forList cg.preReset (fun tag ->
              doTagTask i h (tag,ResetGroupStopTask)));
          bSubPat

        | Test _ -> failwith "impossible: doEnter.Test should be unreachable"
          

        (* In Repeat A flushUp history may have multiple futures
           1) It may be at or above the lowBound and be returned as is
           2) It may not be at a fixed high bound and try to accept null and returned
           3) It may not be at a fixed high bound and be held for doEnter
        *)

        | Repeat r ->
          let loopIt hLoop = (* mutate hLoop *)
            doRepTask hLoop (r.repDepth,IncRep r.topCount);
            forList r.resetOrbits (fun o -> doTagTask i hLoop (o,ResetOrbitTask));
            forOpt r.getOrbit (fun o -> doTagTask i hLoop (o,LoopOrbitTask))
          and leaveIt hLeave = (* mutates hLeave *)
            doRepTask hLeave (r.repDepth,LeaveRep);
            forOpt r.getOrbit (fun o -> doTagTask i hLeave (o,LeaveOrbitTask))
          in
          let bRep = flushUp here r.unRep
          and loopNull h = (* does not alter parameter h, copies it *)
            let hLoop = copyHistory h in
            (loopIt hLoop; doEnterNull_history here hLoop r.unRep)
          and atLimit = match r.optHiBound with
              None -> (fun _ -> false)
            | Some hi -> (fun h ->  hi = h.repA.(r.repDepth))
          and canExit h = r.lowBound <= h.repA.(r.repDepth)
          in
          let histories = Option.value_map bRep ~default:[] ~f:BundleMap.data
          and loopEnter h =  (* does not mutate h, may copy *)
            if atLimit h
            then None
            else let hLoop = copyHistory h
                 in (loopIt hLoop; Some hLoop)
          and listFlush h = (* does not mutate h, may copy *)
            let self = if canExit h then [h] else [] in
            if atLimit h
            then self
            else Option.value_map (loopNull h) ~default:self ~f:(fun hLoop -> hLoop::self) 
          in 
          let bLoop = 
            match (Core.Core_list.filter_map histories ~f:loopEnter) with
                [] -> None
              | hs -> Some (BundleMap.of_alist_exn (* no collisions possible *)
                              (Core.Core_list.map
                                 hs 
                                 ~f:(fun h -> (h.repA,h))))
          and bFlush =
            match (Core.Core_list.concat_map histories ~f:listFlush) with
                [] -> None
              | hs -> Some (BundleMap.map (* resolve collisions *)
                              (BundleMap.of_alist_multi (* collisions possible *)
                                 (Core.Core_list.map
                                    hs 
                                    ~f:(fun h -> leaveIt h;
                                                 (h.repA,h))))
                              ~f:bestHistoryList)
          in
          (* Cannot have a collision in the add operation *)
          forOpt bLoop (fun b -> m1 := IndexMap.add ~key:r.repAt ~data:b !m1);
          bFlush

      in doPostTag bUp; bUp

  and tryTaskList ((i,c) as here) q = (* no mutation *)
    let (_,pc) = !prev in
    let checkTest (test,(expect,_)) =
      expect = (match test with
          Test_BOL -> pc = newline
        | Test_EOL -> c = newline) in
    let tryNull (testSet,taskList) =
      let pass = match testSet with
          AlwaysTrue -> true
        | AlwaysFalse -> false
        | CheckAll tests -> List.for_all checkTest (WhichTestMap.to_alist tests)
      in if pass then Some taskList else None
    in Core.Core_list.find_map q.nullQ ~f:tryNull

  and doEnterNull_bundle ((i,c) as here) bIn q = (* mutate, do not copy *)
    Option.bind (tryTaskList here q) (fun taskList ->
      Option.bind bIn (fun b ->
        BundleMap.iter ~f:(fun ~key:_ ~data:h ->
          ignore (doTasks i h taskList)) b;
        bIn))

  and doEnterNull_history ((i,c) as here) h q = (* mutate, do not copy *)
    Option.bind (tryTaskList here q) (fun taskList ->
      Some (doTasks i h taskList))

  and doEnter = todo

(*
  and doEnter ((i,c) as here) bIn q =
    if (Some 0 = snd q.takes)
    then false
    else
      begin
        let doPreTag optB =
          forOpt optB (fun bContinue ->
            forOpt q.preTag (fun tag ->
              forBundle bContinue (fun hContinue ->
                doTagTask i hContinue (tag,TagTask))))
        in
        doPreTag bIn;
        match q.unQ with
            Or qs ->
              let descend qChild = doEnter here (copyOptBundle bIn qChild) in
              let progeny = Core.Core_list.map qs ~f:descend in
              List.fold_left (||) false progeny
                
          | Seq (qFront,qBack) ->
            let bMid1 = Option.bind (tryTaskList here q) (fun taskList ->
              Option.bind bIn (fun b ->
                BundleMap.iter ~f:(fun ~key:_ ~data:h ->
                  ignore (doTasks i h taskList)) b;
                bIn))

match tryTaskList here qFront with
                None -> None
              | Some taskList -> 
            let bMid1 = doEnter here bIn qFront
              *)              
                
        
  in todo


