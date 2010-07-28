(* Simulate.ml *)

(* This is trying to take the CorePattern and use this to match against a target string.

   Absolutely NO attempt to be efficient is being made, this is entirely to get the right answer.
   This will be made to work on the test suite before proceeding.
   This will then be used as a basis for comparision to build the NFA matcher.

  XXX todo: rewrite to emplot doTagTask and doRepTask
*)

open CamomileLibrary
open Common
open WhichTest
open Pattern
open ReadPattern
open CorePattern

TYPE_CONV_PATH "Simulate"

type orbitHistory = { mutable inOrbit : bool
                    ; mutable basePos : strIndex
                    ; mutable middlePos : int list
                    ; mutable sortedIndex : int option
                    }

type history = { tagA : int array
               ; repA : int array
               ; orbitA : (int list) array
               }

let copyHistory { tagA=a;repA=b;orbitA=c } = { tagA = Array.copy a
                                             ; repA = Array.copy b
                                             ; orbitA = Array.copy c
                                             }

type simStack = SimReturn | SimEnterAny | SimEnterAccept

type aThread = AT of ((strIndex*uchar)*((strIndex*uchar) list) -> (aThread list))

(* very simple way to comprehend utf8 input string *)
let stringToList : ustring -> (strIndex*uchar) list = fun s ->
  let firstBytePos = UTF8.first s
  and lastBytePos = UTF8.last s in
  let rec go bytePos = if bytePos <= lastBytePos 
    then let nextBytePos = UTF8.next s bytePos
         in (bytePos,UTF8.look s bytePos) :: go nextBytePos
    else []
  in
  go firstBytePos

let newline = UChar.of_char '\n'

let doTagTask i h (tag,tagTask) = match tagTask with
    TagTask -> h.tagA.(tag) <- i
  | ResetGroupStopTask -> h.tagA.(tag) <- (-1)
  | SetGroupStopTask -> h.tagA.(tag) <- 0
  | ResetOrbitTask -> h.tagA.(tag) <- (-1); h.orbitA.(tag) <- []
  | EnterOrbitTask -> h.tagA.(tag) <- 0; h.orbitA.(tag) <- []
  | LoopOrbitTask -> h.orbitA.(tag) <- i :: h.orbitA.(tag)
  | LeaveOrbitTask -> h.tagA.(tag) <- 1;
    ()

let doRepTask h (tag,repTask) = match repTask with
    IncRep topCount -> h.repA.(tag) <- max topCount (1+h.repA.(tag))
  | LeaveRep -> h.repA.(tag) <- 0

let doTasks i h (tagTasks,repTasks) = 
  List.iter (doTagTask i h) tagTasks;
  List.iter (doRepTask h) repTasks;
  h

let simCP (cr : coreResult) (utf8string : ustring) =
  let xsTop = stringToList utf8string
  and numTags = Array.length cr.tags
  (*  and numGroups = Array.length cr.groups *)
  and root = cr.cp
  and indexAtEnd = UTF8.next utf8string (UTF8.last utf8string)
  in
  let startHistory = { tagA = Array.make numTags (-1)
                     ; repA = Array.make cr.depth 0
                     ; orbitA = Array.make numTags []
                     }
  and winners = ref []
  in
  doTagTask 0 startHistory (0,TagTask);

  let rec dispatch prev rest h = function (* This DOES mutate the history h *) 
    | [] -> doWin prev indexAtEnd h
    | ((command,q)::context) ->
      match (rest,command) with
          ([]         , SimEnterAccept) -> ()
        | ([]         , SimEnterAny)    -> doEnterEnd  prev indexAtEnd h q context
        | ([]         , SimReturn)      -> doReturnEnd prev indexAtEnd h q context
        | (here::ahead, SimEnterAny)    -> doEnterNull prev here ahead h q context;
                                           doEnter     prev here ahead h q context
        | (here::ahead, SimEnterAccept) -> doEnter     prev here ahead h q context
        | (here::ahead, SimReturn)      -> doReturn    prev here ahead h q context
        
  and doWin (_,_) post h = doTagTask post h (1,TagTask); winners := h :: !winners
  and doEnterEnd ((pi,pc) as prev) post h q context = (* This does NOT mutate the history h *)
    let checkTest (test,(expect,_)) =
      expect = (match test with
          Test_BOL -> pc = newline
        | Test_EOL -> true) in
    let tryNull (testSet,taskList) = 
      let pass = match testSet with
          AlwaysTrue -> true
        | AlwaysFalse -> false
          
        | CheckAll tests -> List.for_all checkTest (WhichTestMap.to_alist tests)
      in if pass then let hpass = doTasks post (copyHistory h) taskList
                      in dispatch prev [] hpass context
        else ()
    in List.iter tryNull q.nullQ
  and doReturnEnd ((pi,pc) as prev) post h q context = (* This DOES mutate the history h *)
    Core.Option.iter q.postTag (fun tag -> doTagTask post h (tag,TagTask));
    match q.unQ with
        Repeat r -> doRepTask h (r.repDepth,LeaveRep)
      | CaptureGroup cg ->
        List.iter (fun tag -> doTagTask post h (tag,ResetGroupStopTask)) cg.preReset;
        doTagTask post h (cg.postSet,SetGroupStopTask)
      | _ -> ();
    dispatch prev [] h context
  and doEnterNull ((pi,pc) as prev) ((i,c) as here) ahead h q context = (* This does NOT mutate the history h *)
    let checkTest (test,(expect,_)) =
      expect = (match test with
          Test_BOL -> pc = newline
        | Test_EOL -> c = newline) in
    let tryNull (testSet,taskList) =
      let pass = match testSet with 
          AlwaysTrue -> true
        | AlwaysFalse -> false
        | CheckAll tests -> List.for_all checkTest (WhichTestMap.to_alist tests)
      in if pass then let hpass = doTasks i (copyHistory h) taskList
                      in dispatch prev (here::ahead) hpass context
    in List.iter tryNull q.nullQ;
  and doEnter ((pi,pc) as prev) ((i,c) as here) ahead h q oldContext = (* This DOES mutate the history h *)
    let newContext = (SimReturn,q) :: oldContext in
    Core.Option.iter q.preTag (fun tag -> doTagTask i h (tag,TagTask));
    match q.unQ with
        Or qs -> List.iter (fun q -> doEnter prev here ahead (copyHistory h) q newContext) qs
      | Seq (qFront,qEnd) ->
        doEnterNull prev here ahead h qFront ((SimEnterAccept,qEnd) :: newContext);
        doEnter prev here ahead h qFront ((SimEnterAny,qEnd) :: newContext)
      | Repeat r -> 
        begin
          (* assert h.repA.(r.repDepth) is zero *)
          if h.repA.(r.repDepth) = 0 then () else failwith "impossible: doEnter.Repeat found non-zero h.repA(r.repDepth)";
          doRepTask h (r.repDepth,IncRep r.topCount);
          List.iter (fun o -> doTagTask i h (o,ResetOrbitTask)) r.resetOrbits;
          Core.Option.iter r.getOrbit (fun o -> doTagTask i h (o,EnterOrbitTask));
          doEnter prev here ahead h r.unRep newContext
        end
      | Test _ -> ()
      | OneChar (us,_) when USet.mem c us -> dispatch here ahead h newContext
      | OneChar _ -> ()
      | CaptureGroup cg -> 
        List.iter (fun tag -> doTagTask i h (tag,ResetGroupStopTask)) cg.preReset;
        doEnter prev here ahead h cg.subPat newContext
  and doReturn prev ((i,c) as here) ahead h q context =
    match q.unQ with
        Repeat r -> 
          (* Assert that this only gets run when r.unRep accepted at least one character *)
          let loopContext = (SimReturn,q) :: context
          and soFar = h.repA.(r.repDepth)
          in
          if 0<soFar then () else failwith "impossible: doReturn.Repeat found soFar <= 0";
          let goLoop hLoop =
            doRepTask hLoop (r.repDepth,IncRep topCount);
            List.iter (fun o -> doTagTask i hLoop (o,ResetOrbitTask)) r.resetOrbits;
            Core.Option.iter r.getOrbit (fun o -> doTagTask i hLoop (o,LoopOrbitTask));
            doEnter prev here ahead hLoop r.unRep loopContext
          and goLeave hLeave =
            doRepTask hLeave (r.repDepth,LeaveRep);
            Core.Option.iter r.getOrbit (fun o -> doTagTask i hLeave (o,LeaveOrbitTask));
            Core.Option.iter q.postTag (fun tag -> doTagTask i hLeave (tag,TagTask));
            dispatch prev (here::ahead) hLeave context
          in
          if soFar < r.lowBound then goLoop h
          else
            begin
              match r.optHiBound with
                  Some hi when soFar > hi -> failwith "impossible soFar > hi";
                | Some hi when soFar = hi -> goLeave h
                | _ -> goLeave (copyHistory h); goLoop h
            end
      | CaptureGroup cg ->
        begin
          Core.Option.iter q.postTag (fun tag -> doTagTask i h (tag,TagTask));
          doTagTask i h (cg.postSet,SetGroupStopTask);
          dispatch prev (here::ahead) h context
        end
      | _ ->
        begin
          Core.Option.iter q.postTag (fun tag -> doTagTask i h (tag,TagTask));
          dispatch prev (here::ahead) h context
        end
  in
  ignore (dispatch (-1,newline) xsTop startHistory [(SimEnterAny,root)]);
  ()

