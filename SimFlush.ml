(* SimFlush.ml *)

(*

  A modfied simCont.ml to operate in passes over the tree instead of iterating over history. 

  State between accepting characters is now physically stored in ROneChar locations.  During processing the state is moved/copied to RSeq and RRepeat nodes.  The two phase processing of flushUp and doEnter also used and implements a mutable count of occupied ROneChar leaves to avoid processing parts of the tree that are unoccupied.

  Redefine history to include additinal information needed


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

TYPE_CONV_PATH "SimFlush"

(* Setup storage types for matching data *)

module RepStateID =
struct
  (* RepStateID.t is repA is the count of each tracked repeat, index type is 'rep' *)
  type t = int array with sexp
  type sexpable = t
  let compare = compare
end

module BundleMap = Core.Core_map.Make(RepStateID)

type bundle = history BundleMap.t

let bzero = BundleMap.empty

(* Transforms the histories without affecting the repA key *)
let forBundle (b : bundle) (g : history -> unit) : unit =
  BundleMap.iter b ~f:(fun ~key:_ ~data:h -> g h)

let copyBundle b = BundleMap.of_alist_exn 
  (Core.Core_list.map 
     (BundleMap.to_alist b) 
     ~f:(fun (key,h) -> (key,copyHistory h)))

(* One use mutable exoskeleton for coreQ.
   Primary Storage is in ROneChar, and is set by doEnter.
   Secondary Storage is in RSeq and RRepeat and is set by doReturn.
   The hasBundle field will store the bool from doEnter for later optimization.
 *)

type 'b runPatternB =
  (* Leaf nodes *)
    ROneChar of uset*('b ref)
  | RTest
  (* Tree nodes *)
  | RSeq of 'b runQB*('b ref)*'b runQB
  | ROr of 'b runQB list
  | RCaptureGroup of capGroupQ*'b runQB
  | RRepeat of repeatQ*('b ref)*'b runQB

and 'b runQB = { getCore : coreQ
               ; getRun : 'b runPatternB
               ; mutable numHistories : int
               }

type runPattern = bundle runPatternB
type runQ = bundle runQB

let rec coreToRun (c : coreQ) : runQ =
  let run = match c.unQ with
    | Or qs -> ROr (List.map coreToRun qs)
    | Seq (qFront,qBack) -> RSeq (coreToRun qFront,
                                  ref bzero,
                                  coreToRun qBack)
    | Repeat r -> RRepeat (r,
                           ref bzero,
                           coreToRun r.unRep)
    | Test _ -> RTest
    | OneChar (uc,_) -> ROneChar (uc,ref bzero)
    | CaptureGroup cg -> RCaptureGroup (cg,coreToRun cg.subPat)
  in { getCore = c; getRun = run; numHistories = 0 }

(*
  simFlush : 
     ?prevIn:Common.strIndex * ReadPattern.uchar
  -> CorePattern.coreResult 
  -> SimStep.simFeed
*)
let simFlush ?(prevIn=(-1,newline)) (cr : coreResult) : simFeed =
  let numTags = Array.length cr.tags
  and root = coreToRun cr.cp
  in
  let prev = ref prevIn  (* Anchors can test facts about preceding character *)
  and winners = ref []
  and startHistory = { tagA   = Array.make numTags (-1)
                     ; repA   = Array.make cr.depthCount 0
                     ; orbitA = Array.make cr.orbitCount []
                     }
  in
  let cycle here =
    prev := here;
    let listWinners = !winners in
    winners := [];
    listWinners
  in

  let bestHistory h1 h2 =
    match compareHistory cr.tags h1 h2 with
      | 1 -> h2
      | _ -> h1
  in

  let bestHistoryList hs = match hs with
    | [] -> failwith "bestHistoryList failed with [] parameter"
    | x::xs -> fold_left bestHistory x xs
  in

  (* Transforms the histories shifting the repA key, with collisions possible *)
  let shiftBundle (b : bundle) (g : history -> history) : bundle =
    BundleMap.map
      ~f:bestHistoryList (* resolve list of collisions *)
      (BundleMap.of_alist_multi
         (Core.Core_list.map
            (BundleMap.data b) 
            ~f:(fun h ->
              let h' = g h
              in (h'.repA,h'))))

  and assembleBundle (hs : history list) : bundle =
    match hs with
      | [] -> bzero
      | hs ->
        BundleMap.map
          ~f:bestHistoryList (* resolve list of collisions *)
          (BundleMap.of_alist_multi
             (Core.Core_list.map
                hs
                ~f:(fun h ->
                  (h.repA,h))))

  and mergeBundles b1 b2 =
    BundleMap.merge b1 b2 ~f:(fun ~key:_ opt12 -> 
      match opt12 with
        | `Both (h1,h2) -> Some (bestHistory h1 h2)
        | `Left h1 -> Some h1
        | `Right h2 -> Some h2)
  in

  let (mergeBundleList : bundle list -> bundle) = function
    | [] -> bzero
    | (b::bs) -> List.fold_left mergeBundles b bs
  in

  let rec nextStep = function
    | StepChar ((i,_c) as here) ->
      forBundle (flushUp here root) (doWin i);
      forOpt (tryTaskList here root) (fun taskList ->
        let h = copyHistory startHistory in
        doTagTask i h (0,TagTask);
        doWin i (doTasks i h taskList));
      let h = copyHistory startHistory in
      doTagTask i h (0,TagTask);
      let b = BundleMap.singleton h.repA h in
      ignore (doEnter here b root);
      cycle here

    | StepEnd indexAtEnd ->
      forBundle (flushUpEnd indexAtEnd root) (doWin indexAtEnd);
      forOpt (tryTaskListEnd indexAtEnd root) (fun taskList ->
        let h = copyHistory startHistory in
        doTagTask indexAtEnd h (0,TagTask);
        doWin indexAtEnd (doTasks indexAtEnd h taskList));
      cycle (indexAtEnd,newline)

  and doWin i h =
    doTagTask i h (1,TagTask);
    winners := h :: !winners

  and flushUp ((i,_c) as here) (rq : runQ) : bundle =
    if rq.numHistories = 0
    then bzero
    else
      let bUp = match rq.getRun with
        | ROneChar (_uc,stored) ->
          let b = !stored in
          stored := bzero;
          b

        | ROr rqs -> mergeBundleList (List.map (flushUp here) rqs)

        | RCaptureGroup (cg,subRQ) ->
          let bSubPat = flushUp here subRQ in 
          forBundle bSubPat (fun h ->
            doTagTask i h (cg.postSet,SetGroupStopTask));
          bSubPat

        | RTest -> bzero

        | RSeq (rqFront,stored,rqBack) ->
          let bFront = flushUp here rqFront in
          stored := bFront;
          let bBack = flushUp here rqBack in
          Option.value_map (tryTaskList here rqBack)
            ~default:bBack
            ~f:(fun taskList ->
              let bMid = copyBundle bFront in
              (forBundle bMid (fun h ->
                ignore (doTasks i h taskList));
               mergeBundles bMid bBack))


        (* In RRepeat the flushUp history may have multiple futures
           1) It may be at or above the lowBound and leave as is
           2) It may be below the lowBound and not be at a fixed high bound and try to accept null & leave
           3) It may not be at a fixed high bound and be held for doEnter
        *)

        | RRepeat (r,stored,subRQ) ->
          let histories = BundleMap.data (flushUp here subRQ)
          and doOrbit h' task = (fun (t,o) -> doOrbitTask i h' (t,o,task))
          in
          let loopIt hLoop = (* mutate hLoop *)
            doRepTask hLoop (r.repDepth,IncRep r.topCount);
            forList r.resetOrbits (doOrbit hLoop ResetOrbitTask);
            forOpt r.getOrbit (doOrbit hLoop LoopOrbitTask)
          and atLimit = match r.optHiBound with
            | None -> (fun _ -> false)
            | Some hi -> (fun h -> hi = h.repA.(r.repDepth))
          in

          (* Make copies of history on way to putting bLoop in stored *)
          let loopEnter h =  (* does not mutate h, may copy *)
            if atLimit h
            then None
            else let hLoop = copyHistory h
                 in (loopIt hLoop; Some hLoop)
          in
          (* makes copies of histories as needed *)
          let bLoop = assembleBundle (Core.Core_list.filter_map histories ~f:loopEnter)
          in
          stored := bLoop;

          (* Now mutate histories on way out of flushUp *)
          let aboveLow h = r.lowBound <= h.repA.(r.repDepth)
          in
          let loopNull (h : history) : history option = (* mutates h, no need to copy it *)
            Option.map (tryTaskList here subRQ) (fun taskList ->
              loopIt h;
              doTasks i h taskList);
          and leaveIt h = (* mutates h, no need to make copies *)
            doRepTask h (r.repDepth,LeaveRep);
            forOpt r.getOrbit (doOrbit h LeaveOrbitTask)
          in
          let listFlush h = (* mutates h, no need to make copies *)
            if aboveLow h
            then [h]
            else match loopNull h with
              | None -> []
              | Some hLoop -> [hLoop]
          in (* mutates and does not need to make copies *)
          let toLeave = Core.Core_list.concat_map histories ~f:listFlush in
          forList toLeave leaveIt;
          assembleBundle toLeave
      and doPostTag (bContinue : bundle) : unit =
        forOpt rq.getCore.postTag (fun tag ->
          forBundle bContinue (fun hContinue ->
            doTagTask i hContinue (tag,TagTask)))
      in 
      doPostTag bUp;
      bUp

  and doEnter ((i,c) as here) bIn rq : int = (* doEnter returns the total BundleMap.length of the sub-tree *)
    if (Some 0 = snd rq.getCore.takes) || ((rq.numHistories = 0) && (BundleMap.length bIn = 0))
    then 0 (* This short-circuits subtress with only RTest leaves, or with nothing to enter/update *)
    else
      begin
        let doPreTag bContinue =
          forOpt rq.getCore.preTag (fun tag ->
            forBundle bContinue (fun hContinue ->
              doTagTask i hContinue (tag,TagTask)))
        in 
        doPreTag bIn; 
        let n = match rq.getRun with
          | ROneChar (uc,stored) when USet.mem c uc ->
            stored := bIn;
            BundleMap.length bIn
              
          | ROneChar _ -> 0

          | ROr rqs ->
            let accepted = Core.Core_list.map rqs 
              ~f:(fun rqChild ->
                doEnter here (copyBundle bIn) rqChild)
            in
            List.fold_left (+) 0 accepted

          | RTest -> failwith "impossible: doEnter.RTest should be unreachable"

          | RCaptureGroup (cg,subRQ) ->
            forBundle bIn (fun h ->
              forList cg.preReset (fun tag ->
                doTagTask i h (tag,ResetGroupStopTask)));
            doEnter here bIn subRQ

          | RSeq (rqFront,stored,rqBack) ->
            let bMid = Option.value_map (tryTaskList here rqFront)
              ~default:!stored
              ~f:(fun taskList ->
                let bSkip = copyBundle bIn in
                forBundle bSkip (fun h ->
                  ignore (doTasks i h taskList));
                mergeBundles bSkip !stored)
            in
            stored := bzero;
            let nFront = doEnter here bIn rqFront in
            let nBack = doEnter here bMid rqBack in
            nFront+nBack

          | RRepeat (r,stored,subRQ) ->
            let b = mergeBundles !stored
              (shiftBundle bIn (fun h ->
                if h.repA.(r.repDepth) <> 0
                then failwith "impossible: doEnter.Repeat found non-zero h.repA.(r.repDepth)";
                doRepTask h (r.repDepth,IncRep r.topCount);
                h))
            in
            stored := bzero;
            doEnter here b subRQ
              
        (* bIn enter loop *)
        (* merge with stored, clear stored *)
        (* descend into subRQ *)

        in
        rq.numHistories <- n;
        n

      end

  and flushUpEnd i (rq : runQ) : bundle =
    let bUp = match rq.getRun with
      (* These four cases are mostly the same as flushUp *)
      | ROneChar (_uc,stored) ->
        let b = !stored in
        stored := bzero;
        b

      | ROr rqs -> mergeBundleList (List.map (flushUpEnd i) rqs)

      | RCaptureGroup (cg,subRQ) ->
        let bSubPat = flushUpEnd i subRQ in 
        forBundle bSubPat (fun h ->
          doTagTask i h (cg.postSet,SetGroupStopTask));
        bSubPat

      | RTest -> bzero

      (* These two cases set stored to bzero *)
      | RSeq (rqFront,stored,rqBack) ->
        stored := bzero;
        let bBack = flushUpEnd i rqBack in
        Option.value_map (tryTaskListEnd i rqBack)
          ~default:bBack
          ~f:(fun taskList ->
            let bFront = flushUpEnd i rqFront in
            (forBundle bFront (fun h ->
              ignore (doTasks i h taskList));
             mergeBundles bFront bBack))

      | RRepeat (r,stored,subRQ) ->
        stored := bzero;
        let histories = BundleMap.data (flushUpEnd i subRQ)
        and doOrbit h' task = (fun (t,o) -> doOrbitTask i h' (t,o,task))
        in
        let loopIt hLoop = (* mutate hLoop *)
          doRepTask hLoop (r.repDepth,IncRep r.topCount);
          forList r.resetOrbits (doOrbit hLoop ResetOrbitTask);
          forOpt r.getOrbit (doOrbit hLoop LoopOrbitTask)
        in
        
        (* Now mutate histories on way out of flushUp *)
        let aboveLow h = r.lowBound <= h.repA.(r.repDepth)
        in
        let loopNull (h : history) : history option = (* mutates h, no need to copy it *)
          Option.map (tryTaskListEnd i subRQ) (fun taskList ->
            loopIt h;
            doTasks i h taskList)
        and leaveIt h = (* mutates h, no need to make copies *)
          doRepTask h (r.repDepth,LeaveRep);
          forOpt r.getOrbit (doOrbit h LeaveOrbitTask)
        in
        let listFlush h = (* mutates h, no need to make copies *)
          if aboveLow h then [h]
          else match loopNull h with
            | None -> []
            | Some hLoop -> [hLoop]
        in
        let toLeave = Core.Core_list.concat_map histories ~f:listFlush in
        forList toLeave leaveIt;
        assembleBundle toLeave

    and doPostTag (bContinue : bundle) : unit =
      forOpt rq.getCore.postTag (fun tag ->
        forBundle bContinue (fun hContinue ->
          doTagTask i hContinue (tag,TagTask)))
    in 
    doPostTag bUp;
    bUp

  and tryTaskList ((i,c) as _here) rq = (* no mutation *)
    let (_,pc) = !prev in
    let checkTest (test,(expect,_)) =
      expect = (match test with
        | Test_BOL -> pc = newline
        | Test_EOL -> c = newline) in
    let tryNull (testSet,taskList) =
      let pass = match testSet with
        | AlwaysTrue -> true
        | AlwaysFalse -> false
        | CheckAll tests -> List.for_all checkTest (WhichTestMap.to_alist tests)
      in if pass then Some taskList else None
    in Core.Core_list.find_map ~f:tryNull rq.getCore.nullQ

  and tryTaskListEnd i rq = (* no mutation *)
    let (_,pc) = !prev in
    let checkTest (test,(expect,_)) =
      expect = (match test with
        | Test_BOL -> pc = newline
        | Test_EOL -> true) in
    let tryNull (testSet,taskList) =
      let pass = match testSet with
        | AlwaysTrue -> true
        | AlwaysFalse -> false
        | CheckAll tests -> List.for_all checkTest (WhichTestMap.to_alist tests)
      in if pass then Some taskList else None
    in Core.Core_list.find_map ~f:tryNull rq.getCore.nullQ

  in nextStep

let uWrapFlush (cr : coreResult) (text : ustring) : o =
  let indexAtEnd = String.length text
  and textList = stringToList text
  and nextStep = simFlush cr
  and allWins = ref []
  in
  let rec go  = function
    | [] ->
      let wins = nextStep (StepEnd indexAtEnd) in
      allWins := wins @ !allWins;
      List.map (fun h -> (interpretGroups 0 cr.groups h,h))
        (List.sort (compareHistory cr.tags) !allWins)
    | (x::xs) ->
      let wins = nextStep (StepChar x) in
      allWins := wins @ !allWins;
      go xs
  in
  go textList

open Core.Result

let wrapSimFlush (pattern : ustring) (text: ustring) : o =
  match (parseRegex pattern) with
    | Error err -> (*Printf.printf "Error: %s\n" err;*) []
    | Ok p -> let cr = toCorePattern p in uWrapFlush cr text
