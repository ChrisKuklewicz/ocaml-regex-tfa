(* SimRank.ml *)

(*

  A modfied simFlush.ml to rank and compress the orbit log.

Steps in change:

DONE: modify orbit log to hold rank, log, and log length.

for bundles leaving repeats do the cohort classification and  compression to rank and empty log.
Here cohort is based on comparison through entry into repeat (exit is implicitly equal).

add max orbit length tracker in repeat

when sub-pattern is empty of state set max length to zero

when looping in repeat capture new maximum length

add limit paramter

  check for max length over limit (after flush up?) and do collection, cohort classification and
  compression.  Here cohort classification is based on comparison though entry into repeat.

*)

open Sexplib.Std
open Common
open WhichTest
open ReadPattern
open CorePattern

TYPE_CONV_PATH "SimRank"

module ImportSimulate = struct
  include (Simulate :
    sig
      val newline : CamomileLibrary.UChar.t
      val stringToList :
        ReadPattern.ustring -> (strIndex * ReadPattern.uchar) list
      val comparePos : 'a list -> 'a list -> int
(* Need to replace these procedures
      val interpretGroups :
      val compareHistory :
      val doOrbitTask :
      val doTasks :
*)
    end)
end
open ImportSimulate

module ImportSimStep = struct
  include (SimStep :
    sig
      type stepData =
          StepChar of (strIndex * ReadPattern.uchar)
        | StepEnd of strIndex
    end)
end
open ImportSimStep

(* Setup storage types for matching data *)
(* new orbitLog type *)
type orbitLog = { basePos : strIndex    (* probably superfluous *)
                ; ordinal : int option  (* sorted rank *)
                ; loopsCount : int      (* length of loops *)
                ; loops : strIndex list (* each loop *)
                }
with sexp

(* shadow Common.history *)
type history = { tagA : int array
               ; repA : int array
               ; orbitA : (orbitLog option) array
               }
with sexp

(* shadow Common.copyHistory *)
let copyHistory { tagA=a;repA=b;orbitA=c } = { tagA = Array.copy a
                                             ; repA = Array.copy b
                                             ; orbitA = Array.copy c
                                             }


(* Replace Simulate.interpretGroups *)
(* init is offset to allow for restarting strIndex *)
let interpretGroups (init : strIndex) (giA : groupInfo array) (h : history) : groupCap =
  (* Printf.printf "tag array length %d\n" (Array.length h.tagA); *)
  (* Printf.printf "group length %d\n" (1+Array.length giA);*)
  let x = Array.create (1+Array.length giA) (-1,-1) in
  x.(0) <- (init+h.tagA.(0),init+h.tagA.(1));
  forArray giA (fun gi ->
    if h.tagA.(gi.flagTag)=0 then
      if x.(gi.parentIndex) <> (-1,-1) then
        x.(gi.thisIndex) <- (init+h.tagA.(gi.startTag),init+h.tagA.(gi.stopTag));
  );
  x

(* Replace Simulate.compareHistory *)
(* Returns -1 if h1 is better than h2 and +1 if h2 is better than h1 and 0 for ties *)
let compareHistory (ops : tagOP array) : history -> history -> int =
  let bound = Array.length ops in
  fun h1 h2 ->
    if h1.repA <> h2.repA  (* sanity check, this ought to be impossible *)
    then
      let s1 = Sexplib.Sexp.to_string_hum (sexp_of_history h1)
      and s2 = Sexplib.Sexp.to_string_hum (sexp_of_history h2)
      in failwith (Printf.sprintf "Simulate.compareHistory.compareOrbit found h1.repA <> h2.repA\n\
                                   h1 is %s\n\
                                   h2 is %s\n" s1 s2)
    else
      let rec go i =
        if i < bound
        then match ops.(i) with
          | Minimize -> (match compare h1.tagA.(i) h2.tagA.(i) with 0 -> go (1+i) | x -> x)
          | Maximize -> (match compare h2.tagA.(i) h1.tagA.(i) with 0 -> go (1+i) | x -> x)
          | GroupFlag -> go (1+i)
          | Orbit orbitNum ->
            begin
              if h1.tagA.(i) <> h2.tagA.(i)
              then (
                let s1 = Sexplib.Sexp.to_string_hum (sexp_of_history h1)
                and s2 = Sexplib.Sexp.to_string_hum (sexp_of_history h2)
                in 
                  failwith (Printf.sprintf "Simulate.compareHistory.compareOrbit at tag %d expected o1=o2 but %d<>%d here\n\
                                   h1 is %s\n\
                                   h2 is %s\n"
                              i  h1.tagA.(i) h2.tagA.(i) s1 s2));
              match h1.orbitA.(orbitNum), h2.orbitA.(orbitNum) with
                | None, None -> go (1+i)
                | Some h1Log, Some h2Log -> 
                    (match comparePos (Core.Core_list.rev h1Log.loops) (Core.Core_list.rev h2Log.loops) with
                      | 0 -> go (1+i)
                      | x -> x
                    )
                | None, Some o | Some o, None -> (
                    let s1 = Sexplib.Sexp.to_string_hum (sexp_of_history h1)
                    and s2 = Sexplib.Sexp.to_string_hum (sexp_of_history h2)
                    and s3 = Sexplib.Sexp.to_string_hum (sexp_of_orbitLog o)
                    in 
                    failwith (Printf.sprintf "Simulate.compareHistory.compareOrbit at tag %d with tagA %d had a None orbitLog and a Some orbitLog o\n\
                                   h1 is %s\n\
                                   h2 is %s\n\
                                   o is %s\n"
                              i  h1.tagA.(i) s1 s2 s3));

            end
        else 0
      in go 0

(* Replace Simulate.doOrbitTask *)
let emptyOrbitLog = { basePos = (-1)
                    ; ordinal = None
                    ; loopsCount = 0
                    ; loops = [] }

let newOrbitLog i = { emptyOrbitLog with basePos = i }

let appendOrbitLog (i : strIndex) (old : orbitLog) : orbitLog = { old with loopsCount = 1 + old.loopsCount; loops = i :: old.loops }

let assertTagA message h tag value =
  if h.tagA.(tag) <> value
  then failwith (Printf.sprintf "%s h.tagA.(%i) == %i <> %i)" message tag h.tagA.(tag) value)

let doOrbitTask (i : strIndex) (h : history) ((tag : tag),(orbit : orbit),orbitTask) : unit = match orbitTask with
    (* tagA value evolves from -1 to 0 when repeat is first entered, from 0 to 1 when it finally leaves *)

  | ResetOrbitTask -> h.tagA.(tag) <- (-1); h.orbitA.(orbit) <- None
  | EnterOrbitTask -> assertTagA "SimRank.doOrbitTask EnterOrbitTask" h tag (-1);
                      h.tagA.(tag) <-   0;  h.orbitA.(orbit) <- Some (newOrbitLog i)
  | LoopOrbitTask  -> assertTagA "SimRank.doOrbitTask LoopOrbitTask" h tag 0;
    (match h.orbitA.(orbit) with
      | None -> failwith "doOrbitTask LoopOrbitTask on None!"
      | Some old -> h.orbitA.(orbit) <- Some (appendOrbitLog i old))
  | LeaveOrbitTask -> assertTagA "SimRank.doOrbitTask LeaveOrbitTask" h tag 0;
                      h.tagA.(tag) <-   1

(* Replace Simulate.doTasks as it must call the new doOrbitTask *)
let doTagTask (i : strIndex) (h : history) ((tag : tag),tagTask) = match tagTask with
    TagTask -> h.tagA.(tag) <- i

    (* tagA value evolves from -1 to 0 when group is fully captured *)
  | ResetGroupStopTask -> h.tagA.(tag) <- (-1)
  | SetGroupStopTask   -> h.tagA.(tag) <-   0
let doRepTask (h : history) (tag,repTask) = match repTask with
    IncRep topCount -> h.repA.(tag) <- min topCount (1+h.repA.(tag))
  | LeaveRep -> h.repA.(tag) <- 0
(* doTasks is used by the null matching code *)
let doTasks (i : strIndex) (h : history) (tl : taskList) =
  forList tl.tlTag (doTagTask i h);
  forList tl.tlOrbit (doOrbitTask i h);
  forList tl.tlRep (doRepTask h);
  h

module RepStateID =
struct
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
    ROneChar of uset * ('b ref)
  | RTest
  (* Tree nodes *)
  | RSeq of ('b runQB) * ('b ref) * ('b runQB)
  | ROr of ('b runQB) list
  | RCaptureGroup of capGroupQ * ('b runQB)
  | RRepeat of repeatQ * ('b ref) * ('b runQB)

and 'b runQB = { getCore : coreQ
               ; getRun : 'b runPatternB
               ; mutable numHistories : int
               }

type runPattern = bundle runPatternB
type runQ = bundle runQB

let rec coreToRun (c : coreQ) : runQ =
  let run = match c.unQ with
    | Or qs -> ROr (Core.Core_list.map qs ~f:coreToRun)
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


type simFeedRank = stepData -> history list

(* The returned simFeedRank muse be fed zero or more StepChar followed by a single StepEnd.  Entries
   in (history list) at each step are winning histories *)
let simRank ?(prevIn=(-1,newline)) (cr : coreResult) : simFeedRank =
  let numTags = Array.length cr.tags
  and root = coreToRun cr.cp
  in
  let prev = ref prevIn  (* Anchors can test facts about preceding character *)
  and winners = ref []
  and startHistory = { tagA   = Array.make numTags (-1)
                     ; repA   = Array.make cr.depthCount 0
                     ; orbitA = Array.make cr.orbitCount None
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
    | x::xs -> Core.Core_list.fold_left ~f:bestHistory ~init:x xs
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

  let mergeBundleList : bundle list -> bundle = function
    | [] -> bzero
    | (b::bs) -> Core.Core_list.fold_left  ~f:mergeBundles ~init:b bs
  in

  let newHistoryAt i = 
    let h = copyHistory startHistory in
    doTagTask i h (0,TagTask);
    h
  in

  let newBundleAt i =
      let h = newHistoryAt i in
      BundleMap.singleton h.repA h
  in

  (* doPreTag does all the rq.getCore.preTag work on the histories in the bundle *)
  let doPreTag rq bContinue i : unit =
    forOpt rq.getCore.preTag (fun tag ->
      forBundle bContinue (fun hContinue ->
        doTagTask i hContinue (tag,TagTask)))
  (* doPostTag does all the rq.getCore.postTag work on the histories in the bundle *)
  and doPostTag rq bContinue i : unit =
    forOpt rq.getCore.postTag (fun tag ->
      forBundle bContinue (fun hContinue ->
        doTagTask i hContinue (tag,TagTask)))
  in

  (* nextStep is the main entry point for simRank, returns winners that complete prior to a new
     character, or at the end of the search *)
  let rec nextStep = function
    | StepChar ((i,_c) as here) ->
      forBundle (flushUp here root) (doWin i);
      forOpt (tryTaskList here root) (fun taskList ->
        doWin i (doTasks i (newHistoryAt i) taskList));
      let b = newBundleAt i in
      ignore (doEnter here b root);
      cycle here

    | StepEnd indexAtEnd ->
      forBundle (flushUpEnd indexAtEnd root) (doWin indexAtEnd);
      forOpt (tryTaskListEnd indexAtEnd root) (fun taskList ->
        doWin indexAtEnd (doTasks indexAtEnd (newHistoryAt indexAtEnd) taskList));
      cycle (indexAtEnd,newline)

  and doWin i h : unit =
    doTagTask i h (1,TagTask);
    winners := h :: !winners

  (* flushUp moves previous stored threads forward, returning bundle of previously stored histories
     that have now finished matching rq *)
  and flushUp ((i,_c) as here) (rq : runQ) : bundle =
    if rq.numHistories = 0
    then bzero
    else
      let bUp = match rq.getRun with
        | ROneChar (_uc,stored) ->
          let b = !stored in
          stored := bzero;
          b

        | ROr rqs -> mergeBundleList (Core.Core_list.map rqs ~f:(flushUp here))

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
          Core.Option.value_map (tryTaskList here rqBack)
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
          let histories = BundleMap.data (flushUp here subRQ)  (* disassembleBundle*)
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
            Core.Option.map (tryTaskList here subRQ) ~f:(fun taskList ->
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

      in 
      doPostTag rq bUp i;
      bUp

  (* doEnter take the history bundle and tries to get rq to store the character.
     doEnter returns the total number of stored histories in all stored bundles.
     All stored bundles are stored in ROneChar nodes.
  *)
  and doEnter ((i,c) as here) bIn rq : int =
    if (Some 0 = snd rq.getCore.takes) || ((rq.numHistories = 0) && (BundleMap.length bIn = 0))
    then 0 (* This short-circuits subtress with only RTest leaves, or with nothing to enter/update *)
    else
      begin
        doPreTag rq bIn i; 
        let n = match rq.getRun with
          | ROneChar (uc,stored) when CamomileLibrary.USet.mem c uc ->
            stored := bIn;
            BundleMap.length bIn
              
          | ROneChar _ -> 0

          | ROr rqs ->
            let accepted = Core.Core_list.map rqs ~f:(fun rqChild ->
              doEnter here (copyBundle bIn) rqChild)
            in
            Core.Core_list.fold_left ~f:(+) ~init:0 accepted

          | RTest -> failwith "impossible: doEnter.RTest should be unreachable"

          | RCaptureGroup (cg,subRQ) ->
            forBundle bIn (fun h ->
              forList cg.preReset (fun tag ->
                doTagTask i h (tag,ResetGroupStopTask)));
            doEnter here bIn subRQ

          | RSeq (rqFront,stored,rqBack) ->
            let bMid = Core.Option.value_map (tryTaskList here rqFront)
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
            let doOrbit h' task = (fun (t,o) -> doOrbitTask i h' (t,o,task))
            in
            let b = mergeBundles !stored
              (shiftBundle bIn (fun h ->
                if h.repA.(r.repDepth) <> 0
                then failwith "impossible: doEnter.Repeat found non-zero h.repA.(r.repDepth)";
                doRepTask h (r.repDepth,IncRep r.topCount);
                forList r.resetOrbits (doOrbit h ResetOrbitTask);
                forOpt r.getOrbit (doOrbit h EnterOrbitTask);
                h))
            in
            stored := bzero;
            doEnter here b subRQ
              
        in
        rq.numHistories <- n;
        n

      end

  (* flushUpEnd checks previous stored threads, returning those that have finished matching rq *)
  and flushUpEnd indexAtEnd (rq : runQ) : bundle =
    let bUp = match rq.getRun with
      (* These four cases are mostly the same as flushUp *)
      | ROneChar (_uc,stored) ->
        let b = !stored in
        stored := bzero;
        b

      | ROr rqs -> mergeBundleList (Core.Core_list.map rqs ~f:(flushUpEnd indexAtEnd))

      | RCaptureGroup (cg,subRQ) ->
        let bSubPat = flushUpEnd indexAtEnd subRQ in 
        forBundle bSubPat (fun h ->
          doTagTask indexAtEnd h (cg.postSet,SetGroupStopTask));
        bSubPat

      | RTest -> bzero

      (* These two cases set stored to bzero *)
      | RSeq (rqFront,stored,rqBack) ->
        stored := bzero;
        let bBack = flushUpEnd indexAtEnd rqBack in
        Core.Option.value_map (tryTaskListEnd indexAtEnd rqBack)
          ~default:bBack
          ~f:(fun taskList ->
            let bFront = flushUpEnd indexAtEnd rqFront in
            (forBundle bFront (fun h ->
              ignore (doTasks indexAtEnd h taskList));
             mergeBundles bFront bBack))

      | RRepeat (r,stored,subRQ) ->
        stored := bzero;
        let histories = BundleMap.data (flushUpEnd indexAtEnd subRQ)
        and doOrbit h' task = (fun (t,o) -> doOrbitTask indexAtEnd h' (t,o,task))
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
          Core.Option.map (tryTaskListEnd indexAtEnd subRQ) ~f:(fun taskList ->
            loopIt h;
            doTasks indexAtEnd h taskList)
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

    in 
    doPostTag rq bUp indexAtEnd;
    bUp

  (* tryTaskList and tryTaskListEnd see if rq succeeds here without consuming a character *)
  and tryTaskList ((_i,c) as _here) rq : taskList option = (* no mutation *)
    let (_,prevChar) = !prev in
    let checkTest (test,(expect,_)) =
      expect = (match test with
        | Test_BOL -> prevChar = newline
        | Test_EOL -> c = newline) in
    let tryNull (testSet,taskList) =
      let pass = match testSet with
        | AlwaysTrue -> true
        | AlwaysFalse -> false
        | CheckAll tests -> Core.Core_list.for_all (WhichTestMap.to_alist tests) ~f:checkTest
      in if pass then Some taskList else None
    in Core.Core_list.find_map ~f:tryNull rq.getCore.nullQ

  (* tryTaskList and tryTaskListEnd see if rq succeeds here without consuming a character *)
  and tryTaskListEnd _indexAtEnd rq : taskList option = (* no mutation *)
    let (_,prevChar) = !prev in
    let checkTest (test,(expect,_)) =
      expect = (match test with
        | Test_BOL -> prevChar = newline
        | Test_EOL -> true) in
    let tryNull (testSet,taskList) =
      let pass = match testSet with
        | AlwaysTrue -> true
        | AlwaysFalse -> false
        | CheckAll tests -> Core.Core_list.for_all (WhichTestMap.to_alist tests) ~f:checkTest
      in if pass then Some taskList else None
    in Core.Core_list.find_map ~f:tryNull rq.getCore.nullQ

  in nextStep

type o = (groupCap * history) list

let uWrapRank (cr : coreResult) (text : ustring) : o =
  let indexAtEnd = String.length text
  and textList = stringToList text
  and nextStep = simRank cr
  and allWins = ref []
  in
  let rec go  = function
    | [] ->
      let wins = nextStep (StepEnd indexAtEnd) in
      allWins := wins @ !allWins;
      Core.Core_list.map
        (Core.Core_list.sort ~cmp:(compareHistory cr.tags) !allWins)
        ~f:(fun h -> (interpretGroups 0 cr.groups h,h))
    | (x::xs) ->
      let wins = nextStep (StepChar x) in
      allWins := wins @ !allWins;
      go xs
  in
  go textList

(*open Core.Result*)

let wrapSimRank (pattern : ustring) (text: ustring) : o =
  match (parseRegex pattern) with
    | Core.Result.Error _err -> (*Printf.printf "Error: %s\n" err;*) []
    | Core.Result.Ok p -> let cr = toCorePattern p in uWrapRank cr text
