(* SimComp.ml *)

(*

  A modfied simFlush.ml to compress the loop history.

  The overriding goal is to make comparePos from simulate.ml give identical answers without having to keep the full lists.  This is done by pre-emtively sorting the orbitA lists and replacing the lists with a single integer. 

  The compression was previously implement in Haskell regex-tfa Text.Regex.TDFA.NewDFA.Engine and I will try to continue that success.  The idea is to logically partition the histories into groups on the following joint criteria on the Repeat: coreQ.  Note that if coreQ.repeatQ.getOrbit is None then we do not have anything to compress.

  let p = coreQ.repeatQ.patIndex
  let Some o = coreQ.repeatQ.getOrbit which is in 0..(numTags-1) and indexes history.tagA and history.orbitA
  let d = coreQ.repeatQ.repDepth which is in 0..(cr.depthCount-1) and indexes history.repA
  let 
  Note that coreResult.tags.(o) = Orbit

  1: Having identical history.tagA(0) values, since this is quite fixed and easy to check
  2: Having identical "base position", index when entering repeat node (optional?)

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
open List
open Core

TYPE_CONV_PATH "SimComp"

(* Setup storage types for matching data *)

type orbitLog = { basePos : int
                ; ordinal : int option
                ; loops : int list
                }
with sexp

type history = { tagA : int array
               ; repA : int array
               ; orbitA : (orbitLog option) array
               }
with sexp

module RepStateID =
struct
  type t = int array with sexp
  type sexpable = t
  let compare = compare
end

(* TODO split this up into separate functions *)
let doTagTask i h (tag,tagTask) = match tagTask with
    TagTask -> h.tagA.(tag) <- i

    (* tagA value evolves from -1 to 0 when group is fully captured *)
  | ResetGroupStopTask -> h.tagA.(tag) <- (-1)
  | SetGroupStopTask   -> h.tagA.(tag) <-   0

    (* tagA value evolves from -1 to 0 when repeat is first entered, from 0 to 1 when it finally leaves *)
  | ResetOrbitTask -> h.tagA.(tag) <- (-1); h.orbitA.(tag) <- None
  | EnterOrbitTask -> h.tagA.(tag) <-   0;  h.orbitA.(tag) <- { basePos = i; ordinal = None; loops = [] }
  | LeaveOrbitTask -> h.tagA.(tag) <-   1
  | LoopOrbitTask  ->
    begin
      match h.orbitA.(tag) with
        | None -> failwith "impossible: LoopOrbitTask against a None instead of Some orbitA"
        | Some o -> h.orbitA.(tag) <- Some { o with loops = i :: o.loops }
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
type runPattern =
  (* Leaf nodes *)
  | ROneChar of uset*(bundle ref)
  | RTest
  (* Tree nodes *)
  | RSeq of runQ*(bundle ref)*runQ
  | ROr of runQ list
  | RCaptureGroup of capGroupQ*runQ
  | RRepeat of repeatQ*(bundle ref)*runQ

and runQ = { getCore : coreQ
           ; getRun : runPattern
           ; mutable numHistories : int
           }

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
    BundleMap.merge (fun ~key:_ opt1 opt2 -> 
      match (opt1,opt2) with
        | (Some h1, Some h2) -> Some (bestHistory h1 h2)
        | (Some h1, None) -> Some h1
        | (None, Some h2) -> Some h2
        | (None,None) -> None) b1 b2
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

        | RTest _ -> bzero

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
          and loopIt hLoop = (* mutate hLoop *)
            doRepTask hLoop (r.repDepth,IncRep r.topCount);
            forList r.resetOrbits (fun o -> doTagTask i hLoop (o,ResetOrbitTask));
            forOpt r.getOrbit (fun o -> doTagTask i hLoop (o,LoopOrbitTask))
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
          let canExit h = r.lowBound <= h.repA.(r.repDepth)
          in
          let loopNull (h : history) : history option = (* mutates h, no need to copy it *)
            Option.map (tryTaskList here subRQ) (fun taskList ->
              loopIt h;
              doTasks i h taskList);
          and leaveIt h = (* mutates h, no need to make copies *)
            doRepTask h (r.repDepth,LeaveRep);
            forOpt r.getOrbit (fun o -> doTagTask i h (o,LeaveOrbitTask))
          in
          let listFlush h = (* mutates h, no need to make copies *)
            let self = if canExit h then [h] else [] in
            if (not (atLimit h)) && (h.repA.(r.repDepth) < r.lowBound)
            then Option.value_map (loopNull h) ~default:self ~f:(fun hLoop -> hLoop::self)
            else self
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

  and doEnter ((i,c) as here) bIn rq : int =
    if (Some 0 = snd rq.getCore.takes) || ((rq.numHistories = 0) && (BundleMap.cardinal bIn = 0))
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
            BundleMap.cardinal bIn
              
          | ROneChar _ -> 0

          | ROr rqs ->
            let accepted = Core.Core_list.map rqs 
              ~f:(fun rqChild ->
                doEnter here (copyBundle bIn) rqChild)
            in
            List.fold_left (+) 0 accepted

          | RTest _ -> failwith "impossible: doEnter.RTest should be unreachable"

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

      | RTest _ -> bzero

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
        and loopIt hLoop = (* mutate hLoop *)
          doRepTask hLoop (r.repDepth,IncRep r.topCount);
          forList r.resetOrbits (fun o -> doTagTask i hLoop (o,ResetOrbitTask));
          forOpt r.getOrbit (fun o -> doTagTask i hLoop (o,LoopOrbitTask))
        and atLimit = match r.optHiBound with
          | None -> (fun _ -> false)
          | Some hi -> (fun h -> hi = h.repA.(r.repDepth))
        in
        
        (* Now mutate histories on way out of flushUp *)
        let canExit h = r.lowBound <= h.repA.(r.repDepth)
        in
        let loopNull (h : history) : history option = (* mutates h, no need to copy it *)
          Option.map (tryTaskListEnd i subRQ) (fun taskList ->
            loopIt h;
            doTasks i h taskList)
        and leaveIt h = (* mutates h, no need to make copies *)
          doRepTask h (r.repDepth,LeaveRep);
          forOpt r.getOrbit (fun o -> doTagTask i h (o,LeaveOrbitTask))
        in
        let listFlush h = (* mutates h, no need to make copies *)
          let self = if canExit h then [h] else [] in
          if (not (atLimit h)) && (h.repA.(r.repDepth) < r.lowBound)
          then Option.value_map (loopNull h) ~default:self ~f:(fun hLoop -> hLoop::self)
          else self
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
    in Core.Core_list.find_map rq.getCore.nullQ ~f:tryNull

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
    in Core.Core_list.find_map rq.getCore.nullQ ~f:tryNull

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
      Error err -> (*Printf.printf "Error: %s\n" err;*) []
    | Ok p -> let cr = toCorePattern p in uWrapFlush cr text
