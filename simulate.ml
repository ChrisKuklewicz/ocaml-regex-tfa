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
open Nfa

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

type simStack = SimReturn of coreQ | SimEnterAny of coreQ | SimEnterAccept of coreQ

type aThread = AT of ((strIndex*uchar)*((strIndex*uchar) list) -> (aThread list))

(* very simple way to comprehend utf8 input string *)
let stringToList : ustring -> (strIndex*uchar*strIndex) list = fun s ->
  let firstBytePos = UTF8.first s
  and lastBytePos = UTF8.last s in
  let rec go bytePos = if bytePos <= lastBytePos 
    then let nextBytePos = UTF8.next s bytePos
         in (bytePos,UTF8.look s bytePos,nextBytePos) :: go nextBytePos
    else []
  in
  go firstBytePos

let newline = UChar.of_char '\n'

let doTagTask pre post h (tag,tagTask) = match tagTask with
    PreUpdate TagTask -> h.tagA.(tag) <- pre
  | PreUpdate ResetGroupStopTask -> h.tagA.(tag) <- (-1)
  | PreUpdate SetGroupStopTask -> h.tagA.(tag) <- 0
  | PreUpdate ResetOrbitTask -> h.tagA.(tag) <- (-1); h.orbitA.(tag) <- []
  | PreUpdate EnterOrbitTask -> h.tagA.(tag) <- 0; h.orbitA.(tag) <- []
  | PreUpdate LoopOrbitTask -> h.orbitA.(tag) <- pre :: h.orbitA.(tag)
  | PreUpdate LeaveOrbitTask -> h.tagA.(tag) <- 1;
  | PostUpdate TagTask -> h.tagA.(tag) <- post
  | PostUpdate ResetGroupStopTask -> h.tagA.(tag) <- (-1)
  | PostUpdate ResetOrbitTask -> failwith "unreachable doTagTask PostUpdate ResetOrbitTask";
  | PostUpdate EnterOrbitTask -> failwith "unreachable doTagTask PostUpdate EnterOrbitTask";
  | PostUpdate LoopOrbitTask  -> failwith "unreachable doTagTask PostUpdate LoopOrbitTask";
  | PostUpdate LeaveOrbitTask -> failwith "unreachable doTagTask PostUpdate LeaveOrbitTask";

doRepTask = failwith "doRepTask"

let doTasks pre post (tagTasks,repTasks) h = 
  List.iter (doTagTask pre post h) tagTasks;
  List.iter (doRepTask pre post h) repTasks;

let simCP (cr : coreResult) (utf8string : ustring) =
  let xsTop = stringToList utf8string
  and numTags = Array.length cr.tags
(*  and numGroups = Array.length cr.groups *)
  and root = cr.cp
  in
  let startHistory = { tagA = Array.make numTags (-1)
                     ; repA = Array.make cr.depth 0
                     ; orbitA = Array.make numTags []
                     }
  and winners = ref []
  in
  startHistory.tagA.(0) <- 0;

  let rec dispatch prev rest h todo = (* This DOES mutate the history h *) 
    match (rest,todo) with
        (_,[]) -> doWin prev h
      | ([],SimEnterAccept q :: _)    -> ()
      | ([],SimEnterAny q :: context) -> doEnterEnd  prev h q context
      | ([],SimReturn q :: context)   -> doReturnEnd prev h q context
      | (here::ahead,SimEnterAny q :: context)    -> doEnterNull prev here ahead h q context;
                                                     doEnter prev here ahead h q context
      | (here::ahead,SimEnterAccept q :: context) -> doEnter     prev here ahead h q context
      | (here::ahead,SimReturn q :: context)      -> doReturn    prev here ahead h q context

  and doWin (pi,_pc,_) h = h.tagA.(1) <- pi; winners := h :: !winners
  and doEnterEnd ((pi,pc,ppost) as prev) h q context = (* This does NOT mutate the history h *)
    let checkTest (test,(expect,_)) =
      expect = (match test with
          Test_BOL -> pc = newline
        | Test_EOL -> true) in
    let tryNull (testSet,taskList) = 
      let pass = match testSet with
          AlwaysTrue -> true
        | AlwaysFalse -> false

        | CheckAll tests -> List.for_all checkTest (WhichTestMap.to_alist tests)
      in if pass then let hpass = doTasks ppost ppost (copyHistory h) taskList (* second ppost is not used *)
                      in dispatch prev [] hpass context
    in List.iter tryNull q.nullQ
  and doReturnEnd ((pi,pc,ppost) as prev) h q context = (* This DOES mutate the history h *)
    Core.Option.iter q.postTag (fun t -> h.tagA.(t) <- ppost);
    match q.unQ with
        Repeat r ->
            h.repA.(r.repDepth) <- 0;
      | CaptureGroup cg ->
          List.iter (fun t -> h.tagA.(t) <- 0) cg.preReset;
          h.tagA.(cg.postSet) <- 1;
      | _ -> ();
    dispatch prev [] h context
  and doEnterNull ((pi,pc,ppost) as prev) ((i,c,_) as here) ahead h q context = (* This does NOT mutate the history h *)
    let checkTest (test,(expect,_)) =
      expect = (match test with
          Test_BOL -> pc = newline
        | Test_EOL -> c = newline) in
    let tryNull (testSet,taskList) =
      let pass = match testSet with 
          AlwaysTrue -> true
        | AlwaysFalse -> false
        | CheckAll tests -> List.for_all checkTest (WhichTestMap.to_alist tests)
      in if pass then let hpass = doTasks ppost ppost taskList (copyHistory h) (* second ppost is not used *)
                      in dispatch prev (here::ahead) hpass context
    in List.iter tryNull q.nullQ;
  and doEnter ((pi,pc,_) as prev) ((i,c,post) as here) ahead h q oldContext = (* This DOES mutate the history h *)
    let newContext = SimReturn q :: oldContext in
    Core.Option.iter q.preTag (fun t -> h.tagA.(t) <- i);
    match q.unQ with
        Or qs -> List.iter (fun q -> doEnter prev here ahead (copyHistory h) q newContext) qs
      | Seq (qFront,qEnd) ->
        doEnterNull prev here ahead h qFront (SimEnterAccept qEnd :: newContext);
        doEnter prev here ahead h qFront (SimEnterAny qEnd :: newContext)
      | Repeat r -> 
        begin
          (* assert h.repA.(r.repDepth) is zero *)
          if h.repA.(r.repDepth) = 0 then ()
          else failwith "impossible: doEnter.Repeat found non-zero h.repA(r.repDepth)";
          h.repA.(r.repDepth) <- 1 + h.repA.(r.repDepth);
          List.iter (fun o -> h.orbitA.(o) <- []) r.resetOrbits;
          Core.Option.iter r.getOrbit (fun o -> h.orbitA.(o) <- i :: []);
          doEnter prev here ahead h r.unRep newContext
        end
      | Test _ -> ()
      | OneChar (us,_) when USet.mem c us ->
          Core.Option.iter q.postTag (fun t -> h.tagA.(t) <- post);
          dispatch here ahead h oldContext
      | OneChar _ -> ()
      | CaptureGroup cg -> 
        List.iter (fun t -> h.tagA.(t) <- 0) cg.preReset;
        doEnter prev here ahead h cg.subPat newContext
  and doReturn prev ((i,c,_) as here) ahead h q context =
    match q.unQ with
        Repeat r -> 
          (* Assert that this only gets run when r.unRep accepted at least one character *)
          let loopContext = SimReturn q :: context
          and soFar = h.repA.(r.repDepth) in
          if 0<soFar then ()
          else failwith "impossible: doReturn.Repeat found soFar <= 0";
          if soFar < r.lowBound then
            begin
              h.repA.(r.repDepth) <- 1+soFar;
              List.iter (fun o -> h.orbitA.(o) <- []) r.resetOrbits;
              Core.Option.iter r.getOrbit (fun o -> h.orbitA.(o) <- i :: h.orbitA.(o));
              doEnter prev here ahead h r.unRep loopContext
            end
          else
            let maxCount = match r.optHiBound with
                None -> r.lowBound+1
              | Some hi -> hi
            in
            begin
              match r.optHiBound with
                Some hi when soFar > hi -> failwith "impossible soFar > hi";
              | Some hi when soFar = hi ->
                begin
                  h.repA.(r.repDepth) <- 0;
                  Core.Option.iter r.getOrbit (fun o -> h.orbitA.(o) <- i :: h.orbitA.(o));
                  Core.Option.iter q.postTag (fun t -> h.tagA.(t) <- i);
                  dispatch prev (here::ahead) h context
                end
              | _ -> 
                  let hLoop = copyHistory h in
                  begin
                    h.repA.(r.repDepth) <- 0;
                    Core.Option.iter r.getOrbit (fun o -> h.orbitA.(o) <- i :: h.orbitA.(o));
                    Core.Option.iter q.postTag (fun t -> h.tagA.(t) <- i);
                    dispatch prev (here::ahead) h context;
                  end;
                  begin
                    hLoop.repA.(r.repDepth) <- min maxCount (1+soFar);
                    List.iter (fun o -> hLoop.orbitA.(o) <- []) r.resetOrbits;
                    Core.Option.iter r.getOrbit (fun o -> hLoop.orbitA.(o) <- i :: hLoop.orbitA.(o));
                    doEnter prev here ahead hLoop r.unRep loopContext
                  end
            end
      | CaptureGroup cg ->
        begin
          Core.Option.iter q.postTag (fun t -> h.tagA.(t) <- i);
          h.tagA.(cg.postSet) <- 1;
          dispatch prev (here::ahead) h context
        end
      | _ ->
        begin
          Core.Option.iter q.postTag (fun t -> h.tagA.(t) <- i);
          dispatch prev (here::ahead) h context
        end
  in
  ignore (dispatch (-1,newline,0) xsTop startHistory [SimEnterAny root]);
  ()

