(* SimRank.ml *)

(*

  A modfied simFlush.ml to rank and compress the orbit log.

Steps in change:

DONE: modify orbit log to hold rank, log, and log length.

DONE: for bundles leaving repeats do the cohort classification and  compression to rank and empty log.
Here cohort is based on comparison of entry strIndex into repeat (exit is implicitly equal).

DONE: add field for longestOrbit to RRepeat

DONE: when sub-pattern is empty of state set longest to zero, also zero longest in flushUpEnd

DONE: when looping in repeat capture new longest length

DONE: add limit paramter

DONE: check for max length over limit (after flush up?) and do gethering, cohort classification and
  compression.  Here cohort classification is based on comparison though entry into repeat.

*)

open Common
open WhichTest
open ReadPattern
open CorePattern
open Core.Std

TYPE_CONV_PATH "SimRank"

let to_string_hum = Sexplib.Sexp.to_string_hum
let classify = Core_extended.Extended_list.classify

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

let emptyOrbitLog = { basePos = (-1)
                    ; ordinal = None
                    ; loopsCount = 0
                    ; loops = [] }

let newOrbitLog i = { emptyOrbitLog with basePos = i }

let appendOrbitLog (i : strIndex) (old : orbitLog) : orbitLog =
  { old with loopsCount = succ old.loopsCount; loops = i :: old.loops }

let assertTagA message h tag value =
  if h.tagA.(tag) <> value then
    failwith (Printf.sprintf "%s h.tagA.(%i) == %i <> %i)" message tag h.tagA.(tag) value)

let setOrdinal hLog rank = { hLog with ordinal = Some rank; loopsCount = 0; loops = [] }

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
  let x = Array.create (1+Array.length giA) (-1, -1) in
  x.(0) <- (init+h.tagA.(0), init+h.tagA.(1));
  Array.iter giA ~f:(fun gi ->
    if h.tagA.(gi.flagTag)=0 then
      if x.(gi.parentIndex) <> (-1, -1) then
        x.(gi.thisIndex) <- (init+h.tagA.(gi.startTag), init+h.tagA.(gi.stopTag));
  );
  x

(* groups tests consecutive elements and groups runs of "equal" elements into non-empty sublists *)
let group ~eq:eq xsIn =
  let rec go acc nxt x xs =
    match xs with
      | [] -> List.rev (List.rev (x::nxt) :: acc)
      | y :: ys -> if eq x y
        then go acc (x::nxt) y ys
        else go (List.rev (x::nxt) :: acc) [] y ys
  in
  match xsIn with
    | [] -> []
    | (x::xs) -> go [] [] x xs

(* Classify list of history by the basePos of the relevant orbit.
   For each class:
     Classify into sorted groups based on old ordinal and new loops.
     Set the ordinal for each of the groups and clear the loops.
*)
let compressLog histories (_tag, orbit) : unit =
  let getBasePos h = mapOpt (fun hLog -> hLog.basePos) h.orbitA.(orbit)
  and cmpOrdLoops h1 h2 = match h1.orbitA.(orbit), h2.orbitA.(orbit) with
    | None, None | Some _, None | None, Some _ -> failwith "SimRank.compressLog.cmpOrdLoops impossible None"
    | Some h1Log, Some h2Log -> (match compare h1Log.ordinal h2Log.ordinal with
        | 0 -> comparePos (List.rev h1Log.loops) (List.rev h2Log.loops)
        | x -> x)
  and setOrder (rank : int) (h : history) = Option.iter h.orbitA.(orbit)
    ~f:(fun hLog -> h.orbitA.(orbit) <- Some (setOrdinal hLog rank))
      
  in
  let eqOrdLoops h1 h2 = 0 = cmpOrdLoops h1 h2
  in
  let sortClass (entry, hs) = 
    match entry with
      | None -> ()
      | Some _ -> let grouped = group ~eq:eqOrdLoops (List.sort ~cmp:cmpOrdLoops hs)
                  in List.iteri grouped
                       ~f:(fun i hs -> List.iter ~f:(setOrder i) hs)
  and classed = classify ~equal:(=) ~f:getBasePos histories
  in List.iter classed ~f:sortClass

(* cmpLogs is used both in compareHistory and for sorting orbitLogs for compression *)
let cmpLogs tag orbitNum h1 h2 : int =
  begin
    if h1.tagA.(tag) <> h2.tagA.(tag) then (
      let s1 = to_string_hum (sexp_of_history h1)
      and s2 = to_string_hum (sexp_of_history h2)
      in 
      failwith (Printf.sprintf "SimRank.cmpLogs at tag %d expected o1=o2 but %d<>%d here\n\
                                h1 is %s\n\
                                h2 is %s\n"
                  tag  h1.tagA.(tag) h2.tagA.(tag) s1 s2));
    match h1.orbitA.(orbitNum), h2.orbitA.(orbitNum) with
      | None, None -> 0
      | Some h1Log, Some h2Log ->
        let rec matchLogs () =
          match h1Log.ordinal, h2Log.ordinal with
            | None, None -> matchLoops ()
            | Some o1, Some o2 -> matchOrdinals o1 o2
            | None, Some o | Some o, None -> (
              let s1 = to_string_hum (sexp_of_history h1)
              and s2 = to_string_hum (sexp_of_history h2)
              and s3 = to_string_hum (sexp_of_orbitLog h1Log)
              and s4 = to_string_hum (sexp_of_orbitLog h2Log)
              in
              failwith (Printf.sprintf "SimRank.cmpLogs at tag %d with tagA %d had a \"None\" ordinal and a \"Some %d\" ordinal\n\
                                        h1 is %s\n\
                                        h2 is %s\n\
                                        h1Log is %s\n\
                                        h2Log is %s\n"
                          tag  h1.tagA.(tag) o s1 s2 s3 s4));
        and matchOrdinals o1 o2 = (match compare o1 o2 with 0 -> matchLoops () | x -> x)  (* XXX: do I need to negate x ? *)
        and matchLoops () = comparePos (List.rev h1Log.loops) (List.rev h2Log.loops)
        in matchLogs ()
      | None, Some o | Some o, None -> (
        let s1 = to_string_hum (sexp_of_history h1)
        and s2 = to_string_hum (sexp_of_history h2)
        and s3 = to_string_hum (sexp_of_orbitLog o)
        in
        failwith (Printf.sprintf "SimRank.cmpLogs at tag %d with tagA %d had a \"None\" orbitLog and a \"Some o\" orbitLog\n\
                       h1 is %s\n\
                       h2 is %s\n\
                       o is %s\n"
                  tag h1.tagA.(tag) s1 s2 s3));
  end

(* Replace Simulate.compareHistory *)
(* Returns -1 if h1 is better than h2 (h1<h2) and +1 if h2 is better than h1 (h1>h2) and 0 for ties (h1=h2) *)
let compareHistory (ops : tagOP array) : history -> history -> int =
  let bound = Array.length ops in
  fun h1 h2 ->
    if h1.repA <> h2.repA then (* sanity check, this ought to be impossible *)
      let s1 = to_string_hum (sexp_of_history h1)
      and s2 = to_string_hum (sexp_of_history h2)
      in failwith (Printf.sprintf "SimRank.compareHistory.compareOrbit found h1.repA <> h2.repA\n\
                                   h1 is %s\n\
                                   h2 is %s\n" s1 s2)
    else
      let rec go i =
        if bound <= i then 0
        else match ops.(i) with
          | Minimize -> (match compare h1.tagA.(i) h2.tagA.(i) with 0 -> go (succ i) | x -> x)
          | Maximize -> (match compare h2.tagA.(i) h1.tagA.(i) with 0 -> go (succ i) | x -> x)
          | GroupFlag -> go (succ i)
          | Orbit orbitNum -> (match cmpLogs i orbitNum h1 h2 with 0 -> go (succ i) | x -> x)
      in go 0

(* Replace Simulate.doOrbitTask *)
let doOrbitTask (i : strIndex) (h : history) ((tag : tag), (orbit : orbit), orbitTask) : unit = match orbitTask with
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
let doTagTask (i : strIndex) (h : history) ((tag : tag), tagTask) = match tagTask with
    TagTask -> h.tagA.(tag) <- i

    (* tagA value evolves from -1 to 0 when group is fully captured *)
  | ResetGroupStopTask -> h.tagA.(tag) <- (-1)
  | SetGroupStopTask   -> h.tagA.(tag) <-   0
let doRepTask (h : history) (tag, repTask) = match repTask with
    IncRep topCount -> h.repA.(tag) <- min topCount (succ h.repA.(tag))
  | LeaveRep -> h.repA.(tag) <- 0
(* doTasks is used by the null matching code *)
let doTasks (i : strIndex) (h : history) (tl : taskList) =
  List.iter tl.tlTag ~f:(doTagTask i h);
  List.iter tl.tlOrbit ~f:(doOrbitTask i h);
  List.iter tl.tlRep ~f:(doRepTask h);
  h

module RepStateID =
struct
  type t = int array with sexp
  type sexpable = t
  let compare = compare
end

module BundleMap = Map.Make(RepStateID)

type bundle = history BundleMap.t

let bzero = BundleMap.empty

(* Transforms the histories without affecting the repA key *)
let bundle_iter (b : bundle) ~f:(g : history -> unit) : unit =
  BundleMap.iter b ~f:(fun ~key:_ ~data:h -> g h)

let copyBundle b = BundleMap.of_alist_exn 
  (List.map 
     (BundleMap.to_alist b) 
     ~f:(fun (key, h) -> (key, copyHistory h)))

(* One use mutable exoskeleton for coreQ.
   Primary Storage is in ROneChar, and is set by doEnter.
   Secondary Storage is in RSeq and RRepeat and is set by doReturn.
   The hasBundle field will store the bool from doEnter for later optimization.
 *)

type longestOrbit = int ref

type 'b runPatternB =
  (* Leaf nodes *)
    ROneChar of uset * ('b ref)
  | RTest
  (* Tree nodes *)
  | RSeq of ('b runQB) * ('b ref) * ('b runQB)
  | ROr of ('b runQB) list
  | RCaptureGroup of capGroupQ * ('b runQB)
  | RRepeat of repeatQ * ('b ref) * longestOrbit * ('b runQB)

and 'b runQB = { getCore : coreQ
               ; getRun : 'b runPatternB
               ; mutable numHistories : int
               }

type runPattern = bundle runPatternB
type runQ = bundle runQB

let rec coreToRun (c : coreQ) : runQ =
  let run = match c.unQ with
    | Or qs -> ROr (List.map qs ~f:coreToRun)
    | Seq (qFront, qBack) -> RSeq (coreToRun qFront,
                                  ref bzero,
                                  coreToRun qBack)
    | Repeat r -> RRepeat (r,
                           ref bzero,
                           ref 0,
                           coreToRun r.unRep)
    | Test _ -> RTest
    | OneChar (uc, _) -> ROneChar (uc, ref bzero)
    | CaptureGroup cg -> RCaptureGroup (cg, coreToRun cg.subPat)
  in { getCore = c; getRun = run; numHistories = 0 }

type simFeedRank = stepData -> history list

let param_LimitLoopsCount = ref 1

(* The returned simFeedRank muse be fed zero or more StepChar followed by a single StepEnd.  Entries
   in (history list) at each step are winning histories *)
let simRank ?(prevIn=(-1, newline)) (cr : coreResult) : simFeedRank =
  let numTags = Array.length cr.tags
  and root = coreToRun cr.cp
  and limitLoopsCount = !param_LimitLoopsCount
  and prev = ref prevIn  (* Anchors can test facts about preceding character *)
  and winners = ref []
  and bestHistory h1 h2 =
    match compareHistory cr.tags h1 h2 with
      | 1 -> h2
      | _ -> h1
  in

  let startHistory = { tagA   = Array.create numTags (-1)
                     ; repA   = Array.create cr.depthCount 0
                     ; orbitA = Array.create cr.orbitCount None
                     }
  and bestHistoryList hs = match hs with
    | [] -> failwith "bestHistoryList failed with [] parameter"
    | x::xs -> List.fold_left ~f:bestHistory ~init:x xs
  and cycle here =
    prev := here;
    let listWinners = !winners in
    winners := [];
    listWinners
  in

  let newHistoryAt i = 
    let h = copyHistory startHistory in
    doTagTask i h (0, TagTask);
    h
  in

  let newBundleAt i =
      let h = newHistoryAt i in
      BundleMap.singleton h.repA h
  in

  (* Transforms the histories shifting the repA key, with collisions possible *)
  let shiftBundle (b : bundle) (g : history -> history) : bundle =
    BundleMap.map
      ~f:bestHistoryList (* resolve list of collisions *)
      (BundleMap.of_alist_multi
         (List.map
            (BundleMap.data b) 
            ~f:(fun h ->
              let h' = g h
              in (h'.repA, h'))))

  and assembleBundle (hs : history list) : bundle =
    match hs with
      | [] -> bzero
      | hs ->
        BundleMap.map
          ~f:bestHistoryList (* resolve list of collisions *)
          (BundleMap.of_alist_multi
             (List.map
                hs
                ~f:(fun h ->
                  (h.repA, h))))

  and mergeBundles b1 b2 =
    BundleMap.merge b1 b2 ~f:(fun ~key:_ opt12 -> 
      match opt12 with
        | `Both (h1, h2) -> Some (bestHistory h1 h2)
        | `Left h1 -> Some h1
        | `Right h2 -> Some h2)
  in

  let mergeBundleList : bundle list -> bundle = function
    | [] -> bzero
    | (b::bs) -> List.fold_left  ~f:mergeBundles ~init:b bs
  in

  let rec gatherBundles rq = 
    match rq.getRun with
      | ROneChar (_uc, stored) -> BundleMap.data !stored
      | RTest -> []
      | RSeq (rqFront, _stored, rqBack) -> List.append (gatherBundles rqFront) (gatherBundles rqBack)
      | ROr rqs -> List.concat_map rqs ~f:gatherBundles
      | RCaptureGroup (_cq, subRQ) -> gatherBundles subRQ
      | RRepeat (_r, _stored, _longest, subRQ) -> gatherBundles subRQ
  in

  (* doPreTag does all the rq.getCore.preTag work on the histories in the bundle *)
  let doPreTag rq bContinue i : unit =
    Option.iter rq.getCore.preTag ~f:(fun tag ->
      bundle_iter bContinue ~f:(fun hContinue ->
        doTagTask i hContinue (tag, TagTask)))
  (* doPostTag does all the rq.getCore.postTag work on the histories in the bundle *)
  and doPostTag rq bContinue i : unit =
    Option.iter rq.getCore.postTag ~f:(fun tag ->
      bundle_iter bContinue ~f:(fun hContinue ->
        doTagTask i hContinue (tag, TagTask)))
  in

  (* nextStep is the main entry point for simRank, returns winners that complete prior to a new
     character, or at the end of the search *)
  let rec nextStep = function
    | StepChar ((i, _c) as here) ->
      bundle_iter (flushUp here root) ~f:(doWin i);
      Option.iter (tryTaskList here root) ~f:(fun taskList ->
        doWin i (doTasks i (newHistoryAt i) taskList));
      let b = newBundleAt i in
      ignore (doEnter here b root);
      cycle here

    | StepEnd indexAtEnd ->
      bundle_iter (flushUpEnd indexAtEnd root) ~f:(doWin indexAtEnd);
      Option.iter (tryTaskListEnd indexAtEnd root) ~f:(fun taskList ->
        doWin indexAtEnd (doTasks indexAtEnd (newHistoryAt indexAtEnd) taskList));
      cycle (indexAtEnd, newline)

  and doWin i h : unit =
    doTagTask i h (1, TagTask);
    winners := h :: !winners

  (* flushUp moves previous stored threads forward, returning bundle of previously stored histories
     that have now finished matching rq *)
  and flushUp ((i, _c) as here) (rq : runQ) : bundle =
    if rq.numHistories = 0 then bzero
    else
      let bUp = match rq.getRun with
        | ROneChar (_uc, stored) ->
          let b = !stored in
          stored := bzero;
          b

        | ROr rqs -> mergeBundleList (List.map rqs ~f:(flushUp here))

        | RCaptureGroup (cg, subRQ) ->
          let bSubPat = flushUp here subRQ in 
          bundle_iter bSubPat ~f:(fun h ->
            doTagTask i h (cg.postSet, SetGroupStopTask));
          bSubPat

        | RTest -> bzero

        | RSeq (rqFront, stored, rqBack) ->
          let bFront = flushUp here rqFront in
          stored := bFront;
          let bBack = flushUp here rqBack in
          Option.value_map (tryTaskList here rqBack)
            ~default:bBack
            ~f:(fun taskList ->
              let bMid = copyBundle bFront in
              (bundle_iter bMid ~f:(fun h ->
                ignore (doTasks i h taskList));
               mergeBundles bMid bBack))


        (* In RRepeat the flushUp history may have multiple futures
           1) It may be at or above the lowBound and leave as is
           2) It may be below the lowBound and not be at a fixed high bound and try to accept null & leave
           3) It may not be at a fixed high bound and be held for doEnter
        *)
        | RRepeat (r, stored, longest, subRQ) ->
          let histories = BundleMap.data (flushUp here subRQ)  (* disassembleBundle*)
          and doOrbit h' task = (fun (t, o) -> doOrbitTask i h' (t, o, task))
          in
          let loopIt hLoop = (* mutate hLoop *)
            doRepTask hLoop (r.repDepth, IncRep r.topCount);
            List.iter r.resetOrbits ~f:(doOrbit hLoop ResetOrbitTask);
            Option.iter r.getOrbit ~f:(doOrbit hLoop LoopOrbitTask)
          and atLimit = match r.optHiBound with
            | None -> (fun _ -> false)
            | Some hi -> (fun h -> hi = h.repA.(r.repDepth))
          in

          (* Make copies of history on way to putting in stored *)
          let loopThem h =  (* does not mutate h, may copy *)
            if atLimit h then None
            else let hLoop = copyHistory h
                 in (loopIt hLoop; Some hLoop)
          in
          (* makes copies of histories as needed, retrieved by doEnter *)
          stored := assembleBundle (List.filter_map histories ~f:loopThem);
          (* get longest loopsCount from stored histories (do this after loopThem above) *)
          longest := Option.value_map r.getOrbit ~default:0 ~f:(fun (_t, o) ->
              List.fold_left ~init:!longest ~f:max (
                List.map histories ~f:(fun h ->
                  Option.value_map h.orbitA.(o) ~default:0 ~f:(fun hLog ->
                    hLog.loopsCount))));

          (* Now mutate histories on way out of flushUp *)
          let aboveLow h = r.lowBound <= h.repA.(r.repDepth)
          in
          let loopNull (h : history) : history option = (* mutates h, no need to copy it *)
            Option.map (tryTaskList here subRQ) ~f:(fun taskList ->
              loopIt h;
              doTasks i h taskList);
          and leaveIt h = (* mutates h, no need to make copies *)
            doRepTask h (r.repDepth, LeaveRep);
            Option.iter r.getOrbit ~f:(doOrbit h LeaveOrbitTask)
          in
          let listFlush h = (* mutates h, no need to make copies *)
            if aboveLow h then [h]
            else match loopNull h with
              | None -> []
              | Some hLoop -> [hLoop]
          in 
          let toLeave = List.concat_map histories ~f:listFlush in
          (* mutate toLeave, no need to make copies *)
          List.iter toLeave ~f:leaveIt;
          Option.iter r.getOrbit ~f:(compressLog toLeave);
          assembleBundle toLeave

      in 
      doPostTag rq bUp i;
      bUp

  (* doEnter take the history bundle and tries to get rq to store the character.
     doEnter returns the total number of stored histories in all stored bundles.
     All stored bundles are stored in ROneChar nodes.
  *)
  and doEnter ((i, c) as here) bIn rq : int =
    if (Some 0 = snd rq.getCore.takes) || ((rq.numHistories = 0) && (BundleMap.length bIn = 0))
    then 0 (* This short-circuits subtress with only RTest leaves, or with nothing to enter/update *)
    else
      begin
        doPreTag rq bIn i; 
        let n = match rq.getRun with
          | ROneChar (uc, stored) when CamomileLibrary.USet.mem c uc ->
            stored := bIn;
            BundleMap.length bIn
              
          | ROneChar _ -> 0

          | ROr rqs ->
            let accepted = List.map rqs ~f:(fun rqChild ->
              doEnter here (copyBundle bIn) rqChild)
            in
            List.fold_left ~f:(+) ~init:0 accepted

          | RTest -> failwith "impossible: doEnter.RTest should be unreachable"

          | RCaptureGroup (cg, subRQ) ->
            bundle_iter bIn ~f:(fun h ->
              List.iter cg.preReset ~f:(fun tag ->
                doTagTask i h (tag, ResetGroupStopTask)));
            doEnter here bIn subRQ

          (* These two cases (RSeq and RRepeat) set stored to bzero *)
          | RSeq (rqFront, stored, rqBack) ->
            let bMid = Option.value_map (tryTaskList here rqFront)
              ~default:!stored
              ~f:(fun taskList ->
                let bSkip = copyBundle bIn in
                bundle_iter bSkip ~f:(fun h ->
                  ignore (doTasks i h taskList));
                mergeBundles bSkip !stored)
            in
            stored := bzero;
            let nFront = doEnter here bIn rqFront in
            let nBack = doEnter here bMid rqBack in
            nFront+nBack

          (* These two cases (RSeq and RRepeat) set stored to bzero *)
          | RRepeat (r, stored, longest, subRQ) ->
            let doOrbit h' task = (fun (t, o) -> doOrbitTask i h' (t, o, task))
            in
            let b = mergeBundles !stored  (* retrieve what flushUp stored *)
              (shiftBundle bIn (fun h ->
                if h.repA.(r.repDepth) <> 0 then
                  failwith "impossible: doEnter.Repeat found non-zero h.repA.(r.repDepth)";
                doRepTask h (r.repDepth, IncRep r.topCount);
                List.iter r.resetOrbits ~f:(doOrbit h ResetOrbitTask);
                Option.iter r.getOrbit ~f:(doOrbit h EnterOrbitTask);
                h))
            in
            stored := bzero;
            let n = doEnter here b subRQ in
            if n=0 then
              longest := 0 (* sub-pattern is empty, so clear longest orbitLog.loops *)
            else
              if !longest * n > limitLoopsCount then  (* Use worst case *)
                begin
                  Option.iter r.getOrbit ~f:(compressLog (gatherBundles subRQ));
                  longest := 0
                end;
            n

        in
        rq.numHistories <- n;
        n

      end

  (* flushUpEnd checks previous stored threads, returning those that have finished matching rq *)
  and flushUpEnd indexAtEnd (rq : runQ) : bundle =
    let bUp = match rq.getRun with
      (* These four cases are mostly the same as flushUpEnd *)
      | ROneChar (_uc, stored) ->
        let b = !stored in
        stored := bzero;
        b

      | ROr rqs -> mergeBundleList (List.map rqs ~f:(flushUpEnd indexAtEnd))

      | RCaptureGroup (cg, subRQ) ->
        let bSubPat = flushUpEnd indexAtEnd subRQ in 
        bundle_iter bSubPat ~f:(fun h ->
          doTagTask indexAtEnd h (cg.postSet, SetGroupStopTask));
        bSubPat

      | RTest -> bzero

      (* These two cases (RSeq and RRepeat) set stored to bzero *)
      | RSeq (rqFront, stored, rqBack) ->
        stored := bzero;
        let bBack = flushUpEnd indexAtEnd rqBack in
        Option.value_map (tryTaskListEnd indexAtEnd rqBack)
          ~default:bBack
          ~f:(fun taskList ->
            let bFront = flushUpEnd indexAtEnd rqFront in
            (bundle_iter bFront ~f:(fun h ->
              ignore (doTasks indexAtEnd h taskList));
             mergeBundles bFront bBack))

      (* These two cases (RSeq and RRepeat) set stored to bzero *)
      | RRepeat (r, stored, longest, subRQ) ->
        stored := bzero;
        longest := 0;
        let histories = BundleMap.data (flushUpEnd indexAtEnd subRQ)
        and doOrbit h' task = (fun (t, o) -> doOrbitTask indexAtEnd h' (t, o, task))
        in
        let loopIt hLoop = (* mutate hLoop *)
          doRepTask hLoop (r.repDepth, IncRep r.topCount);
          List.iter r.resetOrbits ~f:(doOrbit hLoop ResetOrbitTask);
          Option.iter r.getOrbit ~f:(doOrbit hLoop LoopOrbitTask)
        in
        
        (* Now mutate histories on way out of flushUpEnd *)
        let aboveLow h = r.lowBound <= h.repA.(r.repDepth)
        in
        let loopNull (h : history) : history option = (* mutates h, no need to copy it *)
          Option.map (tryTaskListEnd indexAtEnd subRQ) ~f:(fun taskList ->
            loopIt h;
            doTasks indexAtEnd h taskList)
        and leaveIt h = (* mutates h, no need to make copies *)
          doRepTask h (r.repDepth, LeaveRep);
          Option.iter r.getOrbit ~f:(doOrbit h LeaveOrbitTask)
        in
        let listFlush h = (* mutates h, no need to make copies *)
          if aboveLow h then [h]
          else match loopNull h with
            | None -> []
            | Some hLoop -> [hLoop]
        in
        let toLeave = List.concat_map histories ~f:listFlush in
        List.iter toLeave ~f:leaveIt;
        assembleBundle toLeave

    in 
    doPostTag rq bUp indexAtEnd;
    bUp

  (* tryTaskList and tryTaskListEnd see if rq succeeds here without consuming a character *)
  and tryTaskList ((_i, c) as _here) rq : taskList option = (* no mutation *)
    let (_, prevChar) = !prev in
    let checkTest (test, (expect, _)) =
      expect = (match test with
        | Test_BOL -> prevChar = newline
        | Test_EOL -> c = newline) in
    let tryNull (testSet, taskList) =
      let pass = match testSet with
        | AlwaysTrue -> true
        | AlwaysFalse -> false
        | CheckAll tests -> List.for_all (WhichTestMap.to_alist tests) ~f:checkTest
      in if pass then Some taskList else None
    in List.find_map ~f:tryNull rq.getCore.nullQ

  (* tryTaskList and tryTaskListEnd see if rq succeeds here without consuming a character *)
  and tryTaskListEnd _indexAtEnd rq : taskList option = (* no mutation *)
    let (_, prevChar) = !prev in
    let checkTest (test, (expect, _)) =
      expect = (match test with
        | Test_BOL -> prevChar = newline
        | Test_EOL -> true) in
    let tryNull (testSet, taskList) =
      let pass = match testSet with
        | AlwaysTrue -> true
        | AlwaysFalse -> false
        | CheckAll tests -> List.for_all (WhichTestMap.to_alist tests) ~f:checkTest
      in if pass then Some taskList else None
    in List.find_map ~f:tryNull rq.getCore.nullQ

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
      List.map
        (List.sort ~cmp:(compareHistory cr.tags) !allWins)
        ~f:(fun h -> (interpretGroups 0 cr.groups h, h))
    | (x::xs) ->
      let wins = nextStep (StepChar x) in
      allWins := wins @ !allWins;
      go xs
  in
  go textList

(*open Core.Result*)

let wrapSimRank (pattern : ustring) (text: ustring) : o =
  match (parseRegex pattern) with
    | Result.Error _err -> (*Printf.printf "Error: %s\n" err;*) []
    | Result.Ok p -> let cr = toCorePattern p in uWrapRank cr text
