(* SimStep.ml *)

(* 

   A modfied simCP to operate one character at a time.
   
   How to define a state?  After eating a character the "Play" moved the mark just past the
   accepting symbols.

   Post choice: run until just after each char fires.  State can be indexed by the regex position of
   the char.  This is much like the "Play" system.

   Pre choice: run until stuck at a Test or Char, and use their regex position as the state index.
   
   Not reducing it is "post", reducing it all the way is "pre"

   Alternate choice for state index could be the context chain passed to dispatch
   Post-Variant: apply postTag and use next context as state.
   Possibility: Reducing the context chain until the next doEnter (non-empty q)?
   But..doReturn for Repeat will add a new context! heavens no!
   So the idea is to reduce the dispatch contect until a "collection point" of some sort.

   Alternative to the context chains?  Make the cells point to next context bits?

   Next baby step: make the "post choice" work as a DFA.  Proof of concept, comparable with the
   "Play" paper, and may give better ideas for making the actual NFA.


*)

open CamomileLibrary
open Common
open WhichTest
open Pattern
open ReadPattern
open CorePattern
open Simulate

TYPE_CONV_PATH "SimStep"

type hid_key = (patIndex * (int array))
with sexp

module HistoryID : Core.Core_map_intf.Key = 
struct
  type t = hid_key
  type sexpable = t
  let sexp_of_t = sexp_of_hid_key
  let t_of_sexp = hid_key_of_sexp
  let compare = compare
end

module HistMap = Core.Core_map.Make(HistoryID)

type postData = { pHistory : history
                ; pPostTag : tag option
                ; pContext : (simStack*coreQ) list
                }

type histMap = postData HistMap.t

type stepData = StepChar of (strIndex*uchar) | StepEnd of strIndex

type simFeed = ( stepData -> history list )

let simStep  ?(prevIn=(-1,newline)) (cr : coreResult) : simFeed =
  let numTags = Array.length cr.tags
  and root = cr.cp
  in
  let startHistory = { tagA   = Array.make numTags     (-1)
                     ; repA   = Array.make (1+cr.depth)  0
                     ; orbitA = Array.make numTags      []
                     }
  and prev = ref prevIn
  and winners = ref []
  and m1 = ref HistMap.empty
  and m2 = ref HistMap.empty
  in
  let cycle here =
    let sw = !winners
    in
    begin
      prev := here;
      winners := [];
      m1 := !m2;
      m2 := HistMap.empty;
      sw
    end
  in
  let rec nextStep = function
    | StepChar here ->
      spark here;
      HistMap.iter (process here) !m1;
      cycle here
    | StepEnd indexAtEnd -> 
      sparkEnd indexAtEnd;
      HistMap.iter (processEnd indexAtEnd) !m1;
      cycle (indexAtEnd,newline)
  and processEnd indexAtEnd ~key:_ ~data:p =
    forOpt p.pPostTag (fun tag -> doTagTask indexAtEnd p.pHistory (tag,TagTask));
    dispatchEnd indexAtEnd p.pHistory p.pContext
  and dispatchEnd indexAtEnd h context =
    match context with
        [] -> doWin indexAtEnd h
      | ((command,q)::context) ->
        match command with
            SimEnterAccept -> ()
          | SimEnterAny -> doEnterEnd indexAtEnd h q context
          | SimReturn note -> doReturnEnd indexAtEnd h q context note;
  and process ((i,_) as here) ~key:_ ~data:p =
    forOpt p.pPostTag (fun tag -> doTagTask i p.pHistory (tag,TagTask));
    dispatch here p.pHistory p.pContext
  and dispatch ((i,_) as here) h context =
    match context with
        [] -> doWin i h
      | ((command,q)::context) -> 
        match command with
            SimEnterAccept -> doEnter here h q context
          | SimEnterAny -> doEnterNull here h q context; doEnter here h q context
          | SimReturn note -> doReturn here h q context note
  and spark here = doEnterNull here startHistory root []
  and sparkEnd indexAtEnd = doEnterEnd indexAtEnd startHistory root []
  and doWin i h =
    doTagTask i h (1,TagTask); 
    winners := h :: !winners
  and doEnterEnd indexAtEnd h q context =
    let (_,pc) = !prev in
    let checkTest (test,(expect,_)) =
      expect = (match test with
          Test_BOL -> pc = newline
        | Test_EOL -> true) in
    let tryNull (testSet,_taskList) = 
      match testSet with
          AlwaysTrue -> true
        | AlwaysFalse -> false
        | CheckAll tests -> List.for_all checkTest (WhichTestMap.to_alist tests)
    in 
    match Core.Core_list.drop_while ~f:(fun x -> not (tryNull x)) q.nullQ with
        [] -> ()
      | ((_testSet,taskList)::_) ->
        let hpass = doTasks indexAtEnd (copyHistory h) taskList
        in dispatchEnd indexAtEnd hpass context
  and doReturnEnd indexAtEnd h q context note =
    let continue () =
      forOpt q.postTag (fun tag -> doTagTask indexAtEnd h (tag,TagTask));
      dispatchEnd indexAtEnd h context
    in
    match q.unQ with
        Repeat r ->
          let soFar = h.repA.(r.repDepth) in
          if (note = NoNote) && (soFar < r.lowBound)
          then
            begin
              doRepTask h (r.repDepth,IncRep r.topCount);
              forList r.resetOrbits (fun o -> doTagTask indexAtEnd h (o,ResetOrbitTask));
              forOpt r.getOrbit (fun o -> doTagTask indexAtEnd h (o,LoopOrbitTask));
              doEnterEnd indexAtEnd h r.unRep ((SimReturn NoteNoLoop,q) :: context)
            end
          else
            begin
              doRepTask h (r.repDepth,LeaveRep);
              forOpt r.getOrbit (fun o -> doTagTask indexAtEnd h (o,LeaveOrbitTask));
              continue ()
            end
      | CaptureGroup cg -> doTagTask indexAtEnd h (cg.postSet,SetGroupStopTask); continue ()
      | _ -> continue ()
  and doEnterNull ((i,c) as here) h q context =
    let (_,pc) = !prev in
    let checkTest (test,(expect,_)) =
      expect = (match test with
          Test_BOL -> pc = newline
        | Test_EOL -> c = newline) in
    let tryNull (testSet,_taskList) = 
      match testSet with
          AlwaysTrue -> true
        | AlwaysFalse -> false
        | CheckAll tests -> List.for_all checkTest (WhichTestMap.to_alist tests)
    in 
    match Core.Core_list.drop_while ~f:(fun x -> not (tryNull x)) q.nullQ with
        [] -> ()
      | ((_testSet,taskList)::_) ->
        let hpass = doTasks i (copyHistory h) taskList
        in dispatch here hpass context
  and doReturn ((i,c) as here) h q context note =
    let continue hContinue =
      forOpt q.postTag (fun tag -> doTagTask i hContinue (tag,TagTask));
      dispatch here hContinue context
    in
    match q.unQ with
        Repeat r ->
          let soFar = h.repA.(r.repDepth) in
          if soFar<=0 then failwith "impossible: doReturn.Repeat found soFar <= 0";
          let goLoop hLoop =
            doRepTask hLoop (r.repDepth,IncRep r.topCount);
            forList r.resetOrbits (fun o -> doTagTask i hLoop (o,ResetOrbitTask));
            forOpt r.getOrbit (fun o -> doTagTask i hLoop (o,LoopOrbitTask));
            doEnter here hLoop r.unRep ((SimReturn NoNote,q) :: context)
          and goLoopNull hLoop =
            doRepTask hLoop (r.repDepth,IncRep r.topCount);
            forList r.resetOrbits (fun o -> doTagTask i hLoop (o,ResetOrbitTask));
            forOpt r.getOrbit (fun o -> doTagTask i hLoop (o,LoopOrbitTask));
            doEnterNull here hLoop r.unRep ((SimReturn NoteNoLoop,q) :: context)
          and goLeave hLeave =
            doRepTask hLeave (r.repDepth,LeaveRep);
            forOpt r.getOrbit (fun o -> doTagTask i hLeave (o,LeaveOrbitTask));
            continue hLeave
          in
          if note = NoteNoLoop then goLeave h
          else if soFar < r.lowBound 
          then (goLoopNull (copyHistory h); goLoop h)
          else
            begin
              match r.optHiBound with
                  Some hi when soFar > hi -> failwith "impossible soFar > hi";
                | Some hi when soFar = hi -> goLeave h
                | _ -> goLeave (copyHistory h); goLoop h
            end
      | CaptureGroup cg -> doTagTask i h (cg.postSet,SetGroupStopTask); continue h
      | _ -> continue h
  and doEnter = failwith "doEnter"
  in
  nextStep
