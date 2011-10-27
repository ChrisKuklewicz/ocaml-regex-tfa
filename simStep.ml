(* SimStep.ml *)

(*

   A modfied simulate to operate one character at a time.

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

open Sexplib.Std
open CamomileLibrary
open Common
open WhichTest
open Pattern
open ReadPattern
open CorePattern
open Simulate
open Core.Result

TYPE_CONV_PATH "SimStep"

let us = CamomileLibrary.UPervasives.escaped_uchar

module HistoryID =
  struct
    type t = (patIndex * int array) with sexp
    type sexpable = t
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
  and numReps = cr.depthCount
  and root = cr.cp
  in

(*  let s = Sexplib.Sexp.to_string_hum (sexp_of_coreResult cr) in Printf.printf "simStep %s\n" s;*)

  let prev = ref prevIn
  and winners = ref []
  and m1 = ref HistMap.empty
  and m2 = ref HistMap.empty
  and startHistory = { tagA   = Array.make numTags (-1)
                     ; repA   = Array.make numReps 0
                     ; orbitA = Array.make numTags []
                     }
  in
  let cycle here =
    prev := here;
    m1 := !m2;
    m2 := HistMap.empty;
    let listWinners = !winners in
    winners := [];
    listWinners
  in

  (* nextStep is the entry point to the regular expression engine.  This assumes that "m2" is an empty
     map when it is called and winners is the empty list.  It first tries spark|End to start a new
     search at the current position. Then it goes through the "m1" map and tries all those steps.
     Then it calls cycle to pull out the batch of winners and replace "m1" with the new "m2" and
     replace "m2" with the empty map. *)
  let rec nextStep = function
    | StepChar ((_i,_c) as here) ->
      (*pr "StepChar (%d,%s)\n" _i (us _c);*)
      spark here;
      (*pr "  _spark done_\n";*)
      HistMap.iter (process here) !m1;
      cycle here

    | StepEnd indexAtEnd -> 
      (*pr "StepEnd %d\n" indexAtEnd;*)
      sparkEnd indexAtEnd;
      (*pr "  _sparkedEnd done_\n";*)
      HistMap.iter (processEnd indexAtEnd) !m1;
      cycle (indexAtEnd,newline)

  (* process(End) are specialize doReturns that revivify the postData and know the previous coreQ
     was a OneChar and they merely perform the proper postTag TagTask and dispatch the context. *)
  and process ((i,_c) as here) ~key:_ ~data:p =
    forOpt p.pPostTag (fun tag -> doTagTask i p.pHistory (tag,TagTask));
    dispatch here p.pHistory p.pContext

  and processEnd indexAtEnd ~key:_ ~data:p =
    forOpt p.pPostTag (fun tag -> doTagTask indexAtEnd p.pHistory (tag,TagTask));
    dispatchEnd indexAtEnd p.pHistory p.pContext

  (* dispatch(End) are, together, very similar to Simulate.dispatch *)
  and dispatch ((i,_c) as here) h context =
    match context with
        [] -> doWin i h
      | ((command,q)::context) ->
        match command with
            SimEnterAccept -> doEnter here h q context
          | SimEnterAny ->
            begin
              doEnterNull here h q context;
              doEnter here h q context
            end
          | SimReturn note -> doReturn here h q context note

  and dispatchEnd indexAtEnd h context =
    match context with
        [] -> doWin indexAtEnd h
      | ((command,q)::context) ->
        match command with
            SimEnterAccept -> ()
          | SimEnterAny -> doEnterNullEnd indexAtEnd h q context
          | SimReturn note -> doReturnEnd indexAtEnd h q context note;

  (* spark(End) turn out to be very simple wrappers for doEnter(Null|End) *)
  and spark ((i,_c) as here) =
    let h = copyHistory startHistory in doTagTask i h (0,TagTask);
    doEnterNull here h root [];
    doEnter here h root []

  and sparkEnd indexAtEnd =
    let h = copyHistory startHistory in
    doTagTask indexAtEnd h (0,TagTask);
    doEnterNullEnd indexAtEnd h root []

  and doWin i h =
    doTagTask i h (1,TagTask);
    (* let s = Sexplib.Sexp.to_string_hum (sexp_of_history h)
       in Printf.printf "  Winner at %d : %s\n" i s;*)
    winners := h :: !winners

  (* doEnterNullEnd, doReturnEnd, there is no doEnterEnd as there are no characters *)

  and doEnterNullEnd indexAtEnd h q context =
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
              (* build special context for nullQ *)
              let returnContext = (SimReturn NoteNoLoop,q) :: context in
              doEnterNullEnd indexAtEnd h r.unRep returnContext
            end
          else
            begin
              doRepTask h (r.repDepth,LeaveRep);
              forOpt r.getOrbit (fun o -> doTagTask indexAtEnd h (o,LeaveOrbitTask));
              continue ()
            end

      | CaptureGroup cg ->
        begin
          doTagTask indexAtEnd h (cg.postSet,SetGroupStopTask);
          continue ()
        end

      | _ -> continue ()

  (* doEnterNull, doEnter, doReturn *)

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

  and doEnter ((i,c) as here) h q context =
    if (Some 0 = snd q.takes)
    then ()
    else
      (* build typical recursing context *)
      let returnContext = (SimReturn NoNote,q) :: context in
      forOpt q.preTag (fun tag -> doTagTask i h (tag,TagTask));
      match q.unQ with
          (* The "fun q ->" is needed to ensure side effect from copyHistory for each invocation *)
          Or qs -> forList qs (fun q -> doEnter here (copyHistory h) q returnContext)
            
        | Seq (qFront,qBack) ->
          (* One can jump from qFront directly to to qBack *)
          (* build special context for nullQ *)
          let acceptBackContext = (SimEnterAccept, qBack) :: returnContext in
          doEnterNull here h qFront acceptBackContext;  (* bounce off doEnterNull -> dispatch -> doEnter *)
          (* dual Any context *)
          let anyBackContext = (SimEnterAny, qBack) :: returnContext in
          doEnter here h qFront anyBackContext;  (* store simEnterAny into history stack, later doEnterNull and doEnter *)

        | Repeat r ->
          begin
            if h.repA.(r.repDepth) <> 0
            then failwith "impossible: doEnter.Repeat found non-zero h.repA.(r.repDepth)";
            doRepTask h (r.repDepth,IncRep r.topCount);
            forList r.resetOrbits (fun o -> doTagTask i h (o,ResetOrbitTask));
            forOpt r.getOrbit (fun o -> doTagTask i h (o,EnterOrbitTask));
            doEnter here h r.unRep returnContext
          end

        | Test _ -> failwith "impossible: doEnter.Test should be unreachable" (* or just be value () *)

        | CaptureGroup cg ->
          forList cg.preReset (fun tag -> doTagTask i h (tag,ResetGroupStopTask));
          doEnter here h cg.subPat returnContext
            
        | OneChar (uc,patIndex) when USet.mem c uc ->
          let hid_key = ((patIndex,h.repA) : HistoryID.t)
          and postData = { pHistory = h
                         ; pPostTag = q.postTag
                         ; pContext = returnContext }
          in
          (* let s1 = Sexplib.Sexp.to_string_hum (HistoryID.sexp_of_t hid_key)
             and s2 = Sexplib.Sexp.to_string_hum (sexp_of_history postData.pHistory)
             in Printf.printf "  OneChar (%d at %s)\n    OneChar %s\n    OneChar %s\n" patIndex (us c) s1 s2; *)
          tryInsertHistory hid_key postData

        | OneChar _ -> ()

  (* The doEnter/OneChar hit above and tryInsertHistory below are the heart of traversing the tree *)

  and tryInsertHistory hid_key postData =
    match HistMap.find !m2 hid_key with
        None -> m2 := HistMap.add ~key:hid_key ~data:postData !m2
      | Some oldData ->
        match compareHistory cr.tags oldData.pHistory postData.pHistory with
          | 1 -> m2 := HistMap.add ~key:hid_key ~data:postData !m2
          | _ -> (*Printf.printf "   OneChar --discarded--";*) ()

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
            (* This is where recursion happens: doReturn into doEnter *)
            (* Note that is replaces the just popped (SimReturn NoNote,q) *)
            (* build typical recursing context *)
            let returnContext = (SimReturn NoNote,q) :: context in
            doEnter here hLoop r.unRep returnContext
          and goLoopNull hLoop =
            doRepTask hLoop (r.repDepth,IncRep r.topCount);
            forList r.resetOrbits (fun o -> doTagTask i hLoop (o,ResetOrbitTask));
            forOpt r.getOrbit (fun o -> doTagTask i hLoop (o,LoopOrbitTask));
            (* This is not acutal recursion, just r.unRep.nullQ *)
            (* build special context for nullQ *)
            let returnContext = (SimReturn NoteNoLoop,q) :: context in
            doEnterNull here hLoop r.unRep returnContext
          and goLeave hLeave =
            (* Can arrive here with soFar < r.lowBound by way of NoNote,goLoopNull,doEnterNull,dispatch,NoteNoLoop
               This is okay since the LeaveRep task will change soFar to 0
            *)
            doRepTask hLeave (r.repDepth,LeaveRep);
            forOpt r.getOrbit (fun o -> doTagTask i hLeave (o,LeaveOrbitTask));
            continue hLeave
          in
          begin
            match note with
                NoteNoLoop -> goLeave h
              | NoNote ->
                if soFar < r.lowBound
                then
                  begin
                    goLoopNull (copyHistory h);
                    goLoop h
                  end
                else
                  begin
                    match r.optHiBound with
                        Some hi when soFar > hi -> failwith "impossible soFar > hi";
                      | Some hi when soFar = hi -> goLeave h
                      | _ ->
                        begin
                          goLeave (copyHistory h);
                          goLoop h
                        end
                  end
          end

      | CaptureGroup cg ->
        doTagTask i h (cg.postSet,SetGroupStopTask);
        continue h

      | _ -> continue h

  in
  nextStep

type o = (Common.groupCap * Common.history) list

let uWrap (cr : coreResult) (text : ustring) : o =
  let indexAtEnd = String.length text
  and textList = stringToList text
  and nextStep = simStep cr
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

let wrapSimStep (pattern : ustring) (text: ustring) : o =
  match (parseRegex pattern) with
      Error err -> (*Printf.printf "Error: %s\n" err;*) []
    | Ok p -> let cr = toCorePattern p in uWrap cr text

let kick s ts =
  match (parseRegex s) with
      Error err -> Printf.printf "Failed to parse: %s\nError message: %s\n" s err;
    | Ok p ->
      let cr = toCorePattern p in
      (* let s = Sexplib.Sexp.to_string_hum (sexp_of_coreResult cr) in Printf.printf "%s\n" s;*)
      forList ts (fun t ->
        Printf.printf "--  --  --  --  --\n";
        Printf.printf "Pattern: %s\n" (show_pattern p);
        Printf.printf "Text: %s\n" t;
        let gh = uWrap cr t in
        forList gh seeSimResult
      );
      Printf.printf "--  --  --  --  --\n";
      ()

let test () =
  begin
    kick "(a*){2}(x)" ["x";"xx";"ax";"aax";"aaax";"aaaax"]
  end
