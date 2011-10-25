(* SaveContext.ml *)
(* In the successful SimStep automaton the context is maintained dynamically, like in Simulate
   back-tracking solver, and saved to the m1/m2 map at each step.

   This simStack context is a reified continuation which is interpreted by dispatch, not a
   continuation function passing style.

   This is handy for the separating the EnterEnd/ReturnEnd handlers from the EnterNull/Enter/Return
   handlers, but this could be accomplished by switching each on the option of having the next
   character.  But leave this reified as it is now.

   For "doEnter" applied to a given (q : coreQ) node the context seems to be unique.  This includes
   the automaton states that are the OneChar leaves.

   The only tricky looking bit is the (SimReturn NoteNoLoop) but this is a convenience to re-use
   doEnterNull and doReturn; this could be made a special case with a bit more code.

   Imagine generating this unique context in an preprocessing step to avoid re-allocating part of it
   during each nextStep. Store it in each node? just in each OneChar?

   The context in doReturn, from SimReturn, is unique except for the NoteNoLoop trick.  And this is
   the SAME CONTEXT as the one in doEnter.

   Thus one could store a (simStack,q) in each node, with None at the root (with option or another
   trick to indicate the root).  This would be the parent context when dispatching from that node.

   doEnterNull would have to take the context as a parameter: dispatch, doEnter(Seq), and
   doEnter(Repeat) all share that code slightly differently.

   Seq seems to split with doEnterNull and doEnter children, but the first is nullQ only.
   Repeat seems to split with doEnterNull(r.unRep),doEnter(r.unRep), and leaving but the first
   is nullQ and the second and third are a split but are handled well internally.

   Thus once SimReturn loses the 'note' parameter and doEnterNull is retracked (use a CPS function?)
   each node can get a reverse pointer to its parent.

   In the "Play" paper style all storage is at the OneChar leaves and the traversal is from the root
   for each step.  This allows collisions to be merged at intermediate Seq/Or/Repeat nodes.  I bet
   this is more efficient that my processing.

   How to obtain this efficiency with my processing?  Try to follow the "Play" paper: Instead of
   attaching the context to the history data, each node has a final field with the history waiting
   to be hoisted by its parent.  Their shift is then doEnter and the smart constructors are
   doReturn.  The oplus is compareHistory.  The otimes gets whacked: outside of shifting Sym (.*.)
   is about sequencing nullQ (already done) or doTask'ing a nullQ to a repMap.  In shifting Sym the
   (.*.) it is about accepting the incoming repMap iff the character is being matched.

   So instead of each history getting a context and using a m1/m2 map to store things, build a new
   tree.  No, build a _pair_ of new trees called runQ with immutable structure.  No, I speculate
   that final in OneChar is the to-be-hoisted-this-time -> to-be-hoisted-next-time value and
   everything else stores the last-hoisted -> this-hoisted value.  Traversing with shift involved
   hosting the old final's (running their doReturn) and then some doEnter-ish stuff, passing to the
   canonical doReturn bit.  I speculate that only one tree is needed.

   How to pick up winning??

   spark
   pull final
   step char w/spark
   pull final
   step char w/spark
   pull final
   step char w/spark
   [pull final]
   end w/spark


   Only Store histories in ROneChar.  The step/pull operation will go in one sweep, passing
   histories up then down to new leaves or all the way up/forward to winning.

   Belowis runPattern, a very quick and very dirty way to get this ticking.

*)

open Sexplib.Std
open Common
open CorePattern
open Simulate
open WhichTest
open CamomileLibrary

type uchar = CamomileLibrary.UChar.t

TYPE_CONV_PATH "SaveContext"

module RepID =
struct
  type t = int array
  with sexp
  type sexpable = t
  let compare = compare
end

module RepMap = Core.Core_map.Make(RepID)

type repMap = history RepMap.t

let copyHistories hmap = RepMap.map ~f:copyHistory hmap
let safeHistories hmap = fun () -> copyHistories hmap

(* The doTagTask never changes the repA field, so the key is stable *)
let doTagTasks' i hmap tt = RepMap.iter ~f:(fun ~key:_ ~data:h -> doTagTask i h tt) hmap
let doTagTasks i hmap tt = doTagTasks' i hmap tt; hmap

(* The following must not change the repA field, so the key is stable *)
let doNullTasks' i hmap tt = RepMap.iter ~f:(fun ~key:_ ~data:h -> ignore (doTasks i h tt)) hmap
let doNullTasks i hmap tt = doNullTasks' i hmap tt; hmap

(* Wrapping the pattern this way is needed so to isolate the mutability when the regular expression
   is being used in multiple parallel searches *)
type runPattern =
    ROr of runQ list
  | RSeq of runQ*(repMap ref)*runQ   (* Hold intermediate refMap between "pull" and "push" *)
  | RRepeat of (repMap ref)*runQ     (* Hold looping refMap between "pull" and "push" *)
  | RTest
  | ROneChar of (repMap ref)
  | RCaptureGroup of runQ

and runQ = { mutable active : bool
           ; mutable final : repMap
           ; contTo : runQ continueTo
           ; mutable getP : runPattern
           ; getQ : coreQ
           }

let rNothing = { active = false
               ; final = RepMap.empty
               ; contTo = ContRoot           (* XXX move this field to core pattern type *)
               ; getP = RTest
               ; getQ = CorePattern.nothing }

let rec convertCore = fun (c : runQ continueTo) q ->
  let self = { rNothing with getQ = q; contTo = c } in
  let _ = match q.unQ with
      Or qs -> self.getP <- ROr (List.map (convertCore (ContReturn self)) qs)
    | Seq (qFront,qEnd) ->
      let rEnd = convertCore c qEnd
      in self.getP <- RSeq (convertCore (ContEnter rEnd) qFront,ref RepMap.empty,rEnd)
    | Repeat r -> self.getP <- RRepeat (ref RepMap.empty,convertCore (ContReturn self) r.unRep)
    | Test _ -> self.getP <- RTest
    | OneChar _ -> let r = ref (RepMap.empty)
                   in self.getP <- ROneChar r
    | CaptureGroup cg -> self.getP <- RCaptureGroup (convertCore (ContReturn self) cg.subPat)
  in self

let convertQR q = convertCore ContRoot q

type stepData = StepChar of (strIndex*uchar) | StepEnd of strIndex
type simFeed = ( stepData -> history list )

   

let runStep ?(prevIn=(-1,newline))  (cr : coreResult) : simFeed =
  let bestHistory h1 h2 = if compareHistory cr.tags h1 h2 <= 0 then h1 else h2 in
  let mergeRepMap = let f ~key:_ bOpt cOpt =
                      match (bOpt,cOpt) with
                          (None,None) -> None
                        | (Some a,Some b) -> Some (bestHistory a b)
                        | (_,None) -> bOpt
                        | (None,_) -> cOpt
                  in RepMap.merge ~f:f
  in
  let fromHistories = function
    | [] -> RepMap.empty
    | (h1::hs) ->
      let alone = RepMap.singleton h1.repA h1
      and alones = List.map (fun h -> RepMap.singleton h.repA h) hs
      in List.fold_left mergeRepMap alone alones
  in
  let numTags = Array.length cr.tags
  and root = convertQR cr.cp
  in
  let prev = ref prevIn
  and freshHistory = safeHistory { tagA   = Array.make numTags      (-1)
                                 ; repA   = Array.make cr.depthCount  0
                                 ; orbitA = Array.make numTags       []
                                 }
  in

  let rec nextStep = function
    | StepChar ((i,_c) as here) -> RepMap.data (dispatch (newSpark i) here root) (* XXX handle spark winning as special case *)
    | StepEnd indexAtEnd ->
      let thin = nullSpark indexAtEnd
      and thick = dispatchEnd indexAtEnd root
      in RepMap.data (mergeRepMap thick thin)

  and newSpark i = let newHistory = freshHistory () in
                   let spark = RepMap.singleton newHistory.repA newHistory in
                   doTagTasks i spark (0,TagTask)
  and nullSpark indexAtEnd = let spark = doNullEnd indexAtEnd (newSpark indexAtEnd) root in
                             doTagTasks indexAtEnd spark (1,TagTask);

  and doNullEnd indexAtEnd hmap rIn = match getTaskList None rIn with
      None -> RepMap.empty
    | Some taskList -> doNullTasks indexAtEnd hmap taskList
  and doNull (i,c) new'hmap rIn = match getTaskList (Some c) rIn with
      None -> RepMap.empty
    | Some taskList -> doNullTasks i (new'hmap ()) taskList
  and getTaskList optC rIn =
    let (_,pc) = !prev in
    let checkTest (test,(expect,_)) =
      expect = (match test with
          Test_BOL -> pc = newline
        | Test_EOL ->
          (match optC with
              None -> true
            | Some c -> c = newline))
    in
    let tryNull (testSet,_taskList) = 
      match testSet with
          AlwaysTrue -> true
        | AlwaysFalse -> false
        | CheckAll tests -> List.for_all checkTest (WhichTestMap.to_alist tests)
    in 
    let q = rIn.getQ in
    match Core.Core_list.drop_while ~f:(fun x -> not (tryNull x)) q.nullQ with
        [] -> None
      | ((_testSet,taskList)::_) -> Some taskList

  (* dispatchEnd fetches the stored states from OneChar and propagates to completion.
     This never splits the history and thus is quite straightforward.
  *)
  and dispatchEnd indexAtEnd rIn =
    let continue h = forOpt rIn.getQ.postTag (fun tag -> doTagTasks' indexAtEnd h (tag,TagTask)); h in
    match rIn.getP with
        ROr rs ->
          let hs = List.map continue (List.map (dispatchEnd indexAtEnd) rs)
          in List.fold_left mergeRepMap RepMap.empty hs
      | RSeq (rFront,_mFromFront,rEnd) -> 
        let h1 = continue (doNullEnd indexAtEnd (dispatchEnd indexAtEnd rFront) rEnd)
        and h2 = continue (dispatchEnd indexAtEnd rEnd)
        in mergeRepMap h1 h2
      | RRepeat (_mLoop,rr) -> 
        (match rIn.getQ.unQ with
            Repeat r ->
              let exitRepeat history = 
                begin
                  doRepTask history (r.repDepth,LeaveRep);
                  forOpt r.getOrbit (fun o -> doTagTask indexAtEnd history (o,LeaveOrbitTask));
                  forOpt rIn.getQ.postTag (fun tag -> doTagTask indexAtEnd history (tag,TagTask));
                  history
                end
              and continueHistory history =
                forOpt rIn.getQ.postTag (fun tag -> doTagTask indexAtEnd history (tag,TagTask));
                history
              and nullTasks = getTaskList None rr
              in
              let processHistory history =
                let soFar = history.repA.(r.repDepth) in
                if soFar = 0 then failwith "impossible: dispatchEnd.RRepeat.processHistory found zero history.repA(r.repDepth)";
                if soFar < r.lowBound
                then match nullTasks with
                    None -> None
                  | Some taskList ->
                    begin
                      doRepTask history (r.repDepth,IncRep r.topCount);
                      forList r.resetOrbits (fun o -> doTagTask indexAtEnd history (o,ResetOrbitTask));
                      forOpt r.getOrbit (fun o -> doTagTask indexAtEnd history (o,LoopOrbitTask));
                      let nullHistory = doTasks indexAtEnd history taskList in
                      Some (continueHistory (exitRepeat nullHistory))
                    end
                else Some (continueHistory (exitRepeat history))
              in
              let childHistories = RepMap.data (dispatchEnd indexAtEnd rr) in
              let doneHistories = Core.Core_list.filter_map childHistories ~f:processHistory 
              in fromHistories doneHistories
          | _ -> failwith "impossible")
      | RTest -> RepMap.empty (* There is no stored state *)
      | RCaptureGroup r -> 
        (match rIn.getQ.unQ with
            CaptureGroup cg -> let h = dispatchEnd indexAtEnd r in 
                               doTagTasks' indexAtEnd h (cg.postSet,SetGroupStopTask);
                               continue h
          | _ -> failwith "impossible")
      | ROneChar mRef -> continue (!mRef)

  and pull ((i,c) as here) rIn =
    let continue h = forOpt rIn.getQ.postTag (fun tag -> doTagTasks' indexAtEnd h (tag,TagTask)); h in
    match rIn.getP with
        ROr rs -> let hs = List.map continue (List.map (pull here) rs)
                  in List.fold_left mergeRepMap RepMap.empty hs
      | RSeq (rFront,mFromFront,rEnd) -> 
        let fromFront = pull here rFront
        and fromEnd = pull here rEnd
        in mFromFront := fromFront; fromEnd
      | RRepeat (mLoop,rr) -> 
        (match rIn.getQ.unQ with
            Repeat r ->
              let exitRepeat history = 
                begin
                  doRepTask history (r.repDepth,LeaveRep);
                  forOpt r.getOrbit (fun o -> doTagTask indexAtEnd history (o,LeaveOrbitTask));
                  forOpt rIn.getQ.postTag (fun tag -> doTagTask indexAtEnd history (tag,TagTask));
                  history
                end
              and continueHistory history =
                forOpt rIn.getQ.postTag (fun tag -> doTagTask indexAtEnd history (tag,TagTask));
                history
              and nullTasks = getTaskList None rr
              in
              let processHistory history =
                let soFar = history.repA.(r.repDepth) in
                if soFar = 0 then failwith "impossible: pull.RRepeat.processHistory found zero history.repA(r.repDepth)";
                let nullHistory = if soFar < r.lowBound
                  then match nullTasks with
                      None -> None
                    | Some taskList ->
                      begin
                        let history2 = copyHistories history;
                        doRepTask history2 (r.repDepth,IncRep r.topCount);
                        forList r.resetOrbits (fun o -> doTagTask indexAtEnd history2 (o,ResetOrbitTask));
                        forOpt r.getOrbit (fun o -> doTagTask indexAtEnd history2 (o,LoopOrbitTask));
                        let nullHistory = doTasks indexAtEnd history2 taskList in
                        Some (continueHistory (exitRepeat nullHistory))
                      end
                else Some (continueHistory (exitRepeat history))
              in
              let childHistories = RepMap.data (dispatchEnd indexAtEnd rr) in
              let doneHistories = Core.Core_list.filter_map childHistories ~f:processHistory 
              in fromHistories doneHistories
          | _ -> failwith "impossible")
      | RTest -> RepMap.empty (* There is no stored state *)
      | RCaptureGroup r -> 
        (match rIn.getQ.unQ with
            CaptureGroup cg -> let h = dispatchEnd indexAtEnd r in 
                               doTagTasks' indexAtEnd h (cg.postSet,SetGroupStopTask);
                               continue h
          | _ -> failwith "impossible")
      | ROneChar mRef -> continue (!mRef)






(* dispatch code below this comment is a failed design.  Laziness would be needed *)




    
  (* Responsibility for incoming skipping rIn is in caller or in dispatch? *)
  (* Make this the responsibility of the CALLER *)
  (* The result of dispatch MUST ONLY be things that accepted a previous character *)
  and dispatch incoming ((i,c) as here) rIn : repMap =
    (* TODO: skip if there is no way to accept a character in rIn *)
    (* TODO: propagate up the active flag *)
    (* TODO: skip if incoming is RepMap.empty and rIn is not active *)
    forOpt rIn.getQ.preTag (fun tag -> doTagTasks' i incoming (tag,TagTask));
    let continue h =
      forOpt rIn.getQ.postTag (fun tag -> doTagTasks' i h (tag,TagTask));
      h
    in
    match rIn.getP with
        ROr rs -> (* The "fun r ->" is needed to ensure side effect from copyHistories for each invocation *)
          let hs = List.map continue (List.map (fun r -> dispatch (copyHistories incoming) here r) rs)
          in List.fold_left mergeRepMap (* XXX *) hs
      | RSeq (rFront,rEnd) ->
        let skipFront = doNull here (safeHistories incoming) rFront in
        let fromFront = dispatch incoming here rFront in
        let fromFrontSkipEnd = continue (doNull here (safeHistories fromFront) rEnd) in
        let fromEnd = continue (dispatch (mergeRepMap skipFront fromFront) here rEnd) in
        mergeRepMap fromFrontSkipEnd fromEnd
      | RRepeat (mLoop,rr) -> 
        (match rIn.getQ.unQ with
            Repeat r -> 
              let processIncoming history =
                doRepTask history (r.repDepth,IncRep r.topCount);
                forList r.resetOrbits (fun o -> doTagTask i history (o,ResetOrbitTask));
                forOpt r.getOrbit (fun o -> doTagTask i history (o,EnterOrbitTask));
                history
              in
              let entering = fromHistories (List.map processIncoming (RepMap.data incoming)) 
              and looping = ref []
              in
              let goLoop hLoop = 
                doRepTask hLoop (r.repDepth,IncRep r.topCount);
                forList r.resetOrbits (fun o -> doTagTask i hLoop (o,ResetOrbitTask));
                forOpt r.getOrbit (fun o -> doTagTask i hLoop (o,LoopOrbitTask));
                looping := hLoop :: !looping;
              in
              let processHistory history =
                let soFar = history.repA.(r.repDepth) in
                if soFar = 0 then failwith "impossible: dispatch.RRepeat.processHistory found non-zero history.repA(r.repDepth)";
                if soFar < r.lowBound
                then (goLoop (copyHistory history); 
                      DFOASO#$I O@#JROWJE JLKW J#$(U (%UIOWEJFL:ASDFJ
        (* XXX FATAL to dispatch XXX dispatching to child needs an input which depends on the looping of the output *)
        )
      | RTest -> RepMap.empty
      | RCaptureGroup r ->
        (match rIn.getQ.unQ with
            CaptureGroup cg ->
              begin
                forList cg.preReset (fun tag -> doTagTasks' i incoming (tag,ResetGroupStopTask));
                let h = continue (dispatch incoming here r) in
                doTagTasks i h (cg.postSet,SetGroupStopTask)
              end
          | _ -> failwith "impossible")
      | ROneChar mRef -> 
        let h = !mRef in
        mRef := (match rIn.getQ.unQ with
            OneChar (uc,_patIndex) when USet.mem c uc -> incoming
          | OneChar _ -> RepMap.empty
          | _ -> failwith "impossible");
        continue h
  in nextStep
