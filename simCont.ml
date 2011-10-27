(* SimCont.ml *)

(*

  A modfied simStep.ml to operate without the context stack.  This requires adding continuation information to the tree.

  State between accepting characters is still logically stored in OneChar locations.

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

TYPE_CONV_PATH "SimCont"

type contData = { cHistory : history
                ; cAt : coreQ
                }

type contMap = contData HistMap.t

let simCont ?(prevIn=(-1,newline)) (cr : coreResult) : simFeed =
  let numTags = Array.length cr.tags
  and numReps = cr.depthCount
  and root = cr.cp
  in
  let prev = ref prevIn  (* Anchors can test facts about preceding character *)
  and winners = ref []
  and m1 = ref HistMap.empty  (* on each iteration vivify possibilities from here *)
  and m2 = ref HistMap.empty  (* while matching store paused possibilities here *)
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

  let rec nextStep = function
    | StepChar ((_i,_c) as here) ->
      spark here;
      HistMap.iter (process here) !m1;
      cycle here

    | StepEnd indexAtEnd ->
      sparkEnd indexAtEnd;
      HistMap.iter (processEnd indexAtEnd) !m1;
      cycle (indexAtEnd,newline)

  and process ((i,_c) as here) ~key:_ ~data:c =
    forOpt c.cAt.postTag (fun tag -> doTagTask i c.cHistory (tag,TagTask));
    dispatch here c.cHistory c.cAt

  and processEnd indexAtEnd ~key:_ ~data:c =
    forOpt c.cAt.postTag (fun tag -> doTagTask indexAtEnd c.cHistory (tag,TagTask));
    dispatchEnd indexAtEnd c.cHistory c.cAt

  and dispatch ((i,_c) as here) h at =
    match at.contTo with
        HowReturn q -> doReturn here h q
      | HowReturnMidSeq q -> doReturnMidSeq here h q
      | HowRoot -> doWin i h
      | HowInvalid -> Printf.printf "impossible: HowInvalid\n"

  and dispatchEnd indexAtEnd h at =
    match at.contTo with
        HowReturn q -> doReturnEnd indexAtEnd h q
      | HowReturnMidSeq q -> doReturnMidSeqEnd indexAtEnd h q
      | HowRoot -> doWin indexAtEnd h
      | HowInvalid -> Printf.printf "impossible: HowInvalid\n"

  and spark ((i,_c) as here) =
    let h = copyHistory startHistory in
    doTagTask i h (0,TagTask);
    begin
      match doEnterNull here h root with
          None -> ()
        | Some hNullWin -> doWin i hNullWin
    end;
    doEnter here h root

  and sparkEnd indexAtEnd =
    let h = copyHistory startHistory in
    doTagTask indexAtEnd h (0,TagTask);
    match doEnterNullEnd indexAtEnd h root with
        None -> ()
      | Some hNullWin -> doWin indexAtEnd hNullWin

  and doWin i h =
    doTagTask i h (1,TagTask);
    winners := h :: !winners

  and doEnterNullEnd indexAtEnd h q =
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
        [] -> None
      | ((_testSet,taskList)::_) ->
        let hpass = doTasks indexAtEnd (copyHistory h) taskList
        in Some hpass

  (* doEnterNull with return a new copy of the history when it returns *)
  and doEnterNull ((i,c) as _here) h q =
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
        [] -> None
      | ((_testSet,taskList)::_) ->
        let hpass = doTasks i (copyHistory h) taskList
        in Some hpass

  and doEnter ((i,c) as here) h q =
    if (Some 0 = snd q.takes)
    then ()
    else
      begin
        forOpt q.preTag (fun tag -> doTagTask i h (tag,TagTask));
        match q.unQ with
            Or qs -> forList qs (fun qChild -> doEnter here (copyHistory h) qChild)

          | Seq (qFront,qBack) ->
            begin
              match doEnterNull here h qFront with
                  None -> ()
                | Some hSkipFront -> doEnter here hSkipFront qBack
            end;
            doEnter here h qFront

          | Repeat r ->
            begin
              if h.repA.(r.repDepth) <> 0
              then failwith "impossible: doEnter.Repeat found non-zero h.repA.(r.repDepth)";
              doRepTask h (r.repDepth,IncRep r.topCount);
              forList r.resetOrbits (fun o -> doTagTask i h (o,ResetOrbitTask));
              forOpt r.getOrbit (fun o -> doTagTask i h (o,EnterOrbitTask));
              doEnter here h r.unRep
            end

          | Test _ ->
            begin
              match q.takes with
                  (lo,None) -> Printf.printf "(%d,None)\n" lo
                | (lo,Some hi) -> Printf.printf "(%d,Some %d)\n" lo hi
            end;
            failwith "impossible: doEnter.Test should be unreachable" (* or just be value () *)

          | CaptureGroup cg ->
            forList cg.preReset (fun tag -> doTagTask i h (tag,ResetGroupStopTask));
            doEnter here h cg.subPat
              
          | OneChar (uc,patIndex) when USet.mem c uc ->
            let hid_key = (patIndex,h.repA)
            and contData = { cHistory = h
                           ; cAt = q }
            in
            tryInsertHistory hid_key contData

          | OneChar _ -> ()
      end

  and tryInsertHistory hid_key contData =
    match HistMap.find !m2 hid_key with
        None -> m2 := HistMap.add ~key:hid_key ~data:contData !m2
      | Some oldData ->
        match compareHistory cr.tags oldData.cHistory contData.cHistory with
          | 1 -> m2 := HistMap.add ~key:hid_key ~data:contData !m2
          | _ -> (*Printf.printf "   OneChar --discarded--";*) ()

  and doReturnEnd indexAtEnd h q =
    let continue hContinue =
      forOpt q.postTag (fun tag -> doTagTask indexAtEnd hContinue (tag,TagTask));
      dispatchEnd indexAtEnd hContinue q
    in
    match q.unQ with
        Repeat r ->
          let soFar = h.repA.(r.repDepth) in
          if soFar <= 0 then failwith "impossible: doReturn.Repeat found soFar <= 0";
          let goLeave hLoop =
            doRepTask hLoop (r.repDepth,LeaveRep);
            forOpt r.getOrbit (fun o -> doTagTask indexAtEnd hLoop (o,LeaveOrbitTask));
            continue hLoop
          in
          let goLoopNull hLoop =
            doRepTask hLoop (r.repDepth,IncRep r.topCount);
            forList r.resetOrbits (fun o -> doTagTask indexAtEnd hLoop (o,ResetOrbitTask));
            forOpt r.getOrbit (fun o -> doTagTask indexAtEnd hLoop (o,LoopOrbitTask));
            (* build special context for nullQ *)
            match doEnterNullEnd indexAtEnd hLoop r.unRep with
                None -> ()
              | Some hLoopNull -> goLeave hLoopNull
          in
          if soFar < r.lowBound
          then goLoopNull h
          else goLeave h

      | CaptureGroup cg ->
        begin
          doTagTask indexAtEnd h (cg.postSet,SetGroupStopTask);
          continue h
        end

      | _ -> continue h

  and doReturn ((i,_c) as here) h q =
    let continue hContinue =
      forOpt q.postTag (fun tag -> doTagTask i hContinue (tag,TagTask));
      dispatch here hContinue q
    in
    match q.unQ with
        Repeat r ->
          let soFar = h.repA.(r.repDepth) in
          if soFar <= 0 then failwith "impossible: doReturn.Repeat found soFar <= 0";
          let goLoop hLoop = (* this is prerequsite for future doReturn *)
            doRepTask hLoop (r.repDepth,IncRep r.topCount);
            forList r.resetOrbits (fun o -> doTagTask i hLoop (o,ResetOrbitTask));
            forOpt r.getOrbit (fun o -> doTagTask i hLoop (o,LoopOrbitTask));
            doEnter here hLoop r.unRep
          and goLeave hLeave = (* no guarantee that soFar is at leat r.lowBound *)
            doRepTask hLeave (r.repDepth,LeaveRep);
            forOpt r.getOrbit (fun o -> doTagTask i hLeave (o,LeaveOrbitTask));
            continue hLeave
          in
          let goLoopNull hLoop =
            doRepTask hLoop (r.repDepth,IncRep r.topCount);
            forList r.resetOrbits (fun o -> doTagTask i hLoop (o,ResetOrbitTask));
            forOpt r.getOrbit (fun o -> doTagTask i hLoop (o,LoopOrbitTask));
            match doEnterNull here hLoop r.unRep with
                None -> ()
              | Some hLoopNull -> goLeave hLoopNull
          in
          begin
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

  and doReturnMidSeq here h q =
    match q.unQ with
        Seq (_qFront,qBack) ->
          begin
            match doEnterNull here h qBack with
                None -> ()
              | Some hSkipBack -> doReturn here hSkipBack q
          end;
          doEnter here h qBack

      | _ -> failwith "impossible: doReturnMidSeq on a non Seq node!"

  and doReturnMidSeqEnd indexAtEnd h q =
    match q.unQ with
        Seq (_qFront,qBack) ->
          begin
            match doEnterNullEnd indexAtEnd h qBack with
                None -> ()
              | Some hSkipBack -> doReturnEnd indexAtEnd hSkipBack q
          end

      | _ -> failwith "impossible: doReturnMidSeqEnd on a non Seq node!"

  in nextStep
        
let uWrapCont (cr : coreResult) (text : ustring) : o =
  let indexAtEnd = String.length text
  and textList = stringToList text
  and nextStep = simCont cr
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

let wrapSimCont (pattern : ustring) (text: ustring) : o =
  match (parseRegex pattern) with
      Error err -> (*Printf.printf "Error: %s\n" err;*) []
    | Ok p -> let cr = toCorePattern p in uWrapCont cr text
