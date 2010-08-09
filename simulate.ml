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
open Core.Result

TYPE_CONV_PATH "Simulate"

let ignore = Pervasives.ignore

type notice = NoNote | NoteNoLoop

type simStack = SimReturn of notice | SimEnterAny | SimEnterAccept

let seeList xs = Sexplib.Sexp.to_string_hum (Sexplib.Conv.sexp_of_list Sexplib.Conv.sexp_of_int xs)

let newline = UChar.of_char '\n'

(* very simple way to comprehend utf8 input string *)
let stringToList : ustring -> (strIndex*uchar) list = fun s ->
  let firstBytePos = UTF8.first s
  and lastBytePos = UTF8.last s in
  let rec go acc bytePos = if bytePos <= lastBytePos 
    then let nextBytePos = UTF8.next s bytePos
         in go ((bytePos,UTF8.look s bytePos) :: acc) nextBytePos
    else List.rev acc
  in
(*  Printf.printf "stringToList %d %d %d\n" firstBytePos lastBytePos (UTF8.length s);*)
  if UTF8.length s > 0 then go [] firstBytePos else []

let rec comparePos a b = match (a,b) with
    ([],[]) -> 0
  | ([],_) -> -1
  | (_,[]) -> 1
  | (x::xs,y::ys) -> (match compare y x with
       0 -> comparePos xs ys
      | z -> z)

(* Returns -1 if h1 is better than h2 and +1 if h2 is better than h1 and 0 for ties *)
let compareHistory ops = 
  let bound = Array.length ops in
  (fun h1 h2 ->
    if h1.repA <> h2.repA then 
      let s1 = Sexplib.Sexp.to_string_hum (sexp_of_history h1)
      and s2 = Sexplib.Sexp.to_string_hum (sexp_of_history h2)
      in failwith (Printf.sprintf "Simulate.compareHistory.compareOrbit found h1.repA <> h2.repA\nh1 is %s\nh2 is %s\n" s1 s2)
    else let rec go i = if i < bound
      then match ops.(i) with
          Minimize -> (match compare h1.tagA.(i) h2.tagA.(i) with 0 -> go (1+i) | x -> x)
        | Maximize -> (match compare h2.tagA.(i) h1.tagA.(i) with 0 -> go (1+i) | x -> x)
        | GroupFlag -> go (1+i)
        | Orbit -> (if h1.tagA.(i) <> h2.tagA.(i)
          then failwith (Printf.sprintf "Simulate.compareHistory.compareOrbit at tag %d expected o1=o2 but %d<>%d\n"
                           i  h1.tagA.(i) h2.tagA.(i))
          else match comparePos (List.rev h1.orbitA.(i)) (List.rev h2.orbitA.(i)) with 0 -> go (1+i) | x -> x)
      else 0
    in go 0)

let interpretGroups init (giA : groupInfo array) h : groupCap = 
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

(* TODO : XXX : once a history comparision is being done consider whether orbits needs to record the
   (-1,0,1) in the main tag array, and consider how to move the orbitA(tag) to a non-wasteful system.

   Consider (Tag tag,TagTask) :: (Orbit tag, EnterOrbitTask) :: (Rep tag, LeaveRep) :: [] system
*)
(* tagA values start and are reset to (-1) *)
(* orbitA values start and are reset to [] *)
let doTagTask i h (tag,tagTask) = match tagTask with
    TagTask -> h.tagA.(tag) <- i

  | ResetGroupStopTask -> h.tagA.(tag) <- (-1)
  | SetGroupStopTask   -> h.tagA.(tag) <-   0

  | ResetOrbitTask -> h.tagA.(tag) <- (-1); h.orbitA.(tag) <- []
  | EnterOrbitTask -> h.tagA.(tag) <-   0;  h.orbitA.(tag) <- []
  | LoopOrbitTask  ->                       h.orbitA.(tag) <- i :: h.orbitA.(tag)
  | LeaveOrbitTask -> h.tagA.(tag) <-   1;
    ()

(* repA values start and are reset to 0 *)
let doRepTask h (tag,repTask) = match repTask with
    IncRep topCount -> h.repA.(tag) <- min topCount (1+h.repA.(tag))
  | LeaveRep -> h.repA.(tag) <- 0

let doTasks i h (tagTasks,repTasks) = 
  forList tagTasks (doTagTask i h);
  forList repTasks (doRepTask h);
  h

let rec simCP ?(prevIn=(-1,newline,0)) (cr : coreResult) (utf8string : ustring) : (groupCap*history) list =
  let (piIn,pcIn,init) = prevIn in
  Printf.printf "simCP %d %d %s\n" piIn init utf8string;
  let xsTop = stringToList utf8string
  and numTags = Array.length cr.tags
  and root = cr.cp
  and indexAtEnd = if UTF8.length utf8string > 0 then UTF8.next utf8string (UTF8.last utf8string) else UTF8.first utf8string
  in
  let startHistory = { tagA   = Array.make numTags     (-1)
                     ; repA   = Array.make (1+cr.depth)  0
                     ; orbitA = Array.make numTags      []
                     }
  and winners = ref []
  in
  let rec dispatch prev rest h contextIn =
    (* dispatch is where the "here" first gets split from the "rest" of the text *)
    (* dispatch is how the simulation walks _up_ or _sideways_ in the tree *)
    (* dispatch is where all winning histories are noticed *)
    (* dispatch is where losing histories are discarded at the end of the text *)
    match contextIn with
        [] -> (match rest with [] -> doWin indexAtEnd h
                     | ((i,_)::_) -> doWin i h)
      | ((command,q)::context) ->
        match (rest,command) with
            ([]         , SimEnterAccept) -> ()
          | ([]         , SimEnterAny)    -> doEnterEnd  prev indexAtEnd h q context
          | ([]         , SimReturn note) -> doReturnEnd prev indexAtEnd h q context note
          | (here::ahead, SimEnterAny)    -> doEnterNull prev here ahead h q context;
                                             doEnter     prev here ahead h q context
          | (here::ahead, SimEnterAccept) -> doEnter     prev here ahead h q context
          | (here::ahead, SimReturn note) -> doReturn    prev here ahead h q context note
        
  and doWin post h = 
    doTagTask post h (1,TagTask); 
(*    let s = Sexplib.Sexp.to_string_hum (sexp_of_history h) in Printf.printf "doWin %s\n" s;*)
    winners := h :: !winners
  and doEnterEnd ((_,pc) as prev) post h q context =
(*    let s = Sexplib.Sexp.to_string_hum (sexp_of_coreQ q) in Printf.printf "doEnterEnd %s\n" s; *)
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
    in forList q.nullQ tryNull
  and doReturnEnd prev post h q context note =
(*    let s = Sexplib.Sexp.to_string_hum (sexp_of_coreQ q) in Printf.printf "doReturnEnd %s\n" s; *)
    let continue () =
      forOpt q.postTag (fun tag -> doTagTask post h (tag,TagTask));
      dispatch prev [] h context
    in
    match q.unQ with
        Repeat r ->
          let soFar = h.repA.(r.repDepth) in
          if (note = NoNote) && (soFar < r.lowBound)
          then
            begin
              doRepTask h (r.repDepth,IncRep r.topCount);
              forList r.resetOrbits (fun o -> doTagTask post h (o,ResetOrbitTask));
              forOpt r.getOrbit (fun o -> doTagTask post h (o,LoopOrbitTask));
              doEnterEnd prev post h r.unRep ((SimReturn NoteNoLoop,q) :: context)
            end
          else
            begin
              doRepTask h (r.repDepth,LeaveRep);
              forOpt r.getOrbit (fun o -> doTagTask post h (o,LeaveOrbitTask));
              continue ()
            end
      | CaptureGroup cg -> doTagTask post h (cg.postSet,SetGroupStopTask); continue ()
      | _ -> continue ()
  and doEnterNull ((_,pc) as prev) ((i,c) as here) ahead h q context =
    (* This does NOT mutate the history h *)
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
    in forList q.nullQ tryNull;
  and doEnter prev ((i,c) as here) ahead h q oldContext =
(*    let s = Sexplib.Sexp.to_string_hum (sexp_of_coreQ q) in Printf.printf "doEnter (%d,%s) %s\n" i (UPervasives.escaped_uchar c) s; *)
    if (Some 0 = snd q.takes) then () else
    let newContext = (SimReturn NoNote,q) :: oldContext in
    forOpt q.preTag (fun tag -> doTagTask i h (tag,TagTask));
    match q.unQ with
        Or qs -> forList qs (fun q -> doEnter prev here ahead (copyHistory h) q newContext)
      | Seq (qFront,qEnd) ->
        doEnterNull prev here ahead h qFront ((SimEnterAccept,qEnd) :: newContext);
        doEnter     prev here ahead h qFront ((SimEnterAny   ,qEnd) :: newContext)
      | Repeat r -> 
        begin
(*          let soFar = h.repA.(r.repDepth) in*)
(*          Printf.printf "doReturn (low %d) (depth %d) (count %d)\n" r.lowBound r.repDepth soFar;*)
          if h.repA.(r.repDepth) <> 0 then failwith "impossible: doEnter.Repeat found non-zero h.repA(r.repDepth)";
          doRepTask h (r.repDepth,IncRep r.topCount);
          forList r.resetOrbits (fun o -> doTagTask i h (o,ResetOrbitTask));
          forOpt r.getOrbit (fun o -> doTagTask i h (o,EnterOrbitTask));
          doEnter prev here ahead h r.unRep newContext
        end
      | Test _ -> () (* unreachable *)
      | OneChar (us,_) when USet.mem c us -> (*Printf.printf "++gulp++ %s\n" (UPervasives.escaped_uchar c);*)
        dispatch here ahead h newContext
      | OneChar _ -> (* Printf.printf "--fail-- %s\n" (UPervasives.escaped_uchar c); *) ()
      | CaptureGroup cg -> 
        forList cg.preReset (fun tag -> doTagTask i h (tag,ResetGroupStopTask));
        doEnter prev here ahead h cg.subPat newContext
  and doReturn prev ((i,c) as here) ahead h q context note=
(*    let s = Sexplib.Sexp.to_string_hum (sexp_of_coreQ q) in Printf.printf "doReturn (%d,%s) %s\n" i (UPervasives.escaped_uchar c) s;*)
    let continue hContinue =
      forOpt q.postTag (fun tag -> doTagTask i hContinue (tag,TagTask));
      dispatch prev (here::ahead) hContinue context
    in
    match q.unQ with
        Repeat r -> 
          let soFar = h.repA.(r.repDepth) in
          (* Printf.printf "doReturn (low %d) (depth %d) (count %d) (note %B)\n" r.lowBound r.repDepth soFar (note=NoteNoLoop);*)
          if soFar<=0 then failwith "impossible: doReturn.Repeat found soFar <= 0";
          let goLoop hLoop =
            doRepTask hLoop (r.repDepth,IncRep r.topCount);
            forList r.resetOrbits (fun o -> doTagTask i hLoop (o,ResetOrbitTask));
            forOpt r.getOrbit (fun o -> doTagTask i hLoop (o,LoopOrbitTask));
            doEnter prev here ahead hLoop r.unRep ((SimReturn NoNote,q) :: context)
          and goLoopNull hLoop =
            doRepTask hLoop (r.repDepth,IncRep r.topCount);
            forList r.resetOrbits (fun o -> doTagTask i hLoop (o,ResetOrbitTask));
            forOpt r.getOrbit (fun o -> doTagTask i hLoop (o,LoopOrbitTask));
            doEnterNull prev here ahead hLoop r.unRep ((SimReturn NoteNoLoop,q) :: context)
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
  in
  doTagTask 0 startHistory (0,TagTask);
  ignore (dispatch (piIn,pcIn) xsTop startHistory [(SimEnterAny,root)]);
  let answer = List.map (fun h -> (interpretGroups init cr.groups h,h)) (List.sort (compareHistory cr.tags) !winners) in
  match answer with
      [] -> if UTF8.length utf8string = 0 then answer
        else let first = UTF8.first utf8string in
             let second = UTF8.next utf8string (UTF8.first utf8string) in
             let shorter = Core.Core_string.drop_prefix utf8string (second-first) in
             simCP ~prevIn:(-1,UTF8.look utf8string first,second-first+init) cr shorter
    | _ -> answer

let seeSimResult ((gA,h) : (groupCap*history)) =
  let s = Sexplib.Sexp.to_string_hum (sexp_of_groupCap gA) in Printf.printf "groupCap %s\n" s;
  let s = Sexplib.Sexp.to_string_hum (sexp_of_history h) in Printf.printf "history %s\n" s;
  ()

let kick s ts =
  match (parseRegex s) with
      Error err -> Printf.printf "Failed to parse: %s\nError message: %s\n" s err;
    | Ok p -> 
      let cr = toCorePattern p in
      (* let s = Sexplib.Sexp.to_string_hum (sexp_of_coreResult cr) in Printf.printf "%s\n" s;*)
      forList ts (fun t->
        Printf.printf "--  --  --  --  --\n";
        Printf.printf "Pattern: %s\n" (show_pattern p);
        Printf.printf "Text: %s\n" t;
        let gh = simCP cr t in
        forList gh seeSimResult
      );
      Printf.printf "--  --  --  --  --\n";
      ()

let test () =
  begin
    kick "(a*){2}(x)" ["x";"xx";"ax";"aax";"aaax";"aaaax"]
  end


let test2 () =
  begin
    kick "(a)(b*)(b{3})" ["a";"ab";"abb";"abbb";"abbbb";"abbbbb"];
    kick "a(b|cd)*e" ["xae";"xxxabe";"xxxxacde";"xxxxxxabbe";"xxxxxxabcde";"xxxxxxxxacdbe"];
    kick "a(b|cd)*e" ["abc";"ae";"abe";"acde";"abbe";"abcde";"acdbe"];
    kick "(abcde|abc|de|ab|cde|a|bc|c)" ["abcde"];
    kick "^(abc|de|ab|cde|a|bc|c)*$" ["abcde"];
    kick "(abc|de|fg|a|bcd|efg|ab|cdef|g|d|cde|cd|ef)*" ["abcdefg"];  (* very good example *)
    kick "^(abc|de|fg|a|bcd|efg|ab|cdef|g|d|cde|cd|ef)*$" ["abcdefg"];  (* very good example *)
    kick "^((abc|de|fg|a|bcd|efg|ab|cdef|g|d|cde|cd|ef)*)*$" ["abcdefg"];  (* good example *)
  end


(*
Pattern: (abc|de|fg|a|bcd|efg|ab|cdef|g|d|cde|cd|ef)*
Text: abcdefg
groupCap ((0 7) (5 7)) history ((tagA (0 7 1 5 0)) (repA (0)) (orbitA ((5 3))))
groupCap ((0 7) (4 7)) history ((tagA (0 7 1 4 0)) (repA (0)) (orbitA ((4 3))))
groupCap ((0 7) (6 7)) history ((tagA (0 7 1 6 0)) (repA (0)) (orbitA ((6 4 3))))
groupCap ((0 7) (6 7)) history ((tagA (0 7 1 6 0)) (repA (0)) (orbitA ((6 2))))
groupCap ((0 7) (5 7)) history ((tagA (0 7 1 5 0)) (repA (0)) (orbitA ((5 2))))
groupCap ((0 7) (4 7)) history ((tagA (0 7 1 4 0)) (repA (0)) (orbitA ((4 2))))
groupCap ((0 7) (6 7)) history ((tagA (0 7 1 6 0)) (repA (0)) (orbitA ((6 4 2))))
groupCap ((0 7) (4 7)) history ((tagA (0 7 1 4 0)) (repA (0)) (orbitA ((4 1))))
groupCap ((0 7) (6 7)) history ((tagA (0 7 1 6 0)) (repA (0)) (orbitA ((6 4 1))))
groupCap ((0 6) (4 6)) history ((tagA (0 6 1 4 0)) (repA (0)) (orbitA ((4 3))))
groupCap ((0 6) (2 6)) history ((tagA (0 6 1 2 0)) (repA (0)) (orbitA ((2))))
groupCap ((0 6) (4 6)) history ((tagA (0 6 1 4 0)) (repA (0)) (orbitA ((4 2))))
groupCap ((0 6) (4 6)) history ((tagA (0 6 1 4 0)) (repA (0)) (orbitA ((4 1))))
groupCap ((0 5) (3 5)) history ((tagA (0 5 1 3 0)) (repA (0)) (orbitA ((3))))
groupCap ((0 5) (2 5)) history ((tagA (0 5 1 2 0)) (repA (0)) (orbitA ((2))))
groupCap ((0 4) (3 4)) history ((tagA (0 4 1 3 0)) (repA (0)) (orbitA ((3))))
groupCap ((0 4) (2 4)) history ((tagA (0 4 1 2 0)) (repA (0)) (orbitA ((2))))
groupCap ((0 4) (1 4)) history ((tagA (0 4 1 1 0)) (repA (0)) (orbitA ((1))))
groupCap ((0 3) (0 3)) history ((tagA (0 3 1 0 0)) (repA (0)) (orbitA (())))
groupCap ((0 2) (0 2)) history ((tagA (0 2 1 0 0)) (repA (0)) (orbitA (())))
groupCap ((0 1) (0 1)) history ((tagA (0 1 1 0 0)) (repA (0)) (orbitA (())))
groupCap ((0 0) (-1 -1)) history ((tagA (0 0 1 -1 -1)) (repA (0)) (orbitA (())))


*)

(*

--  --  --  --  -- 9 ways
Pattern: ^(abc|de|fg|a|bcd|efg|ab|cdef|g|d|cde|cd|ef)*$
Text: abcdefg
groupCap ((0 7) (5 7)) history ((tagA (0 7 1 5 0)) (repA (0)) (orbitA ((5 3))))
groupCap ((0 7) (4 7)) history ((tagA (0 7 1 4 0)) (repA (0)) (orbitA ((4 3))))
groupCap ((0 7) (6 7)) history ((tagA (0 7 1 6 0)) (repA (0)) (orbitA ((6 4 3))))
groupCap ((0 7) (6 7)) history ((tagA (0 7 1 6 0)) (repA (0)) (orbitA ((6 2))))
groupCap ((0 7) (5 7)) history ((tagA (0 7 1 5 0)) (repA (0)) (orbitA ((5 2))))
groupCap ((0 7) (4 7)) history ((tagA (0 7 1 4 0)) (repA (0)) (orbitA ((4 2))))
groupCap ((0 7) (6 7)) history ((tagA (0 7 1 6 0)) (repA (0)) (orbitA ((6 4 2))))
groupCap ((0 7) (4 7)) history ((tagA (0 7 1 4 0)) (repA (0)) (orbitA ((4 1))))
groupCap ((0 7) (6 7)) history ((tagA (0 7 1 6 0)) (repA (0)) (orbitA ((6 4 1))))
--  --  --  --  --

*)

(*
--  --  --  --  -- 48 ways
Pattern: ^((abc|de|fg|a|bcd|efg|ab|cdef|g|d|cde|cd|ef)* )*$
Text: abcdefg
groupCap ((0 7) (0 7) (5 7)) history ((tagA (0 7 1 0 0 1 5 0)) (repA (0 0)) (orbitA (() () () () () (5 3) () ())))
groupCap ((0 7) (0 7) (4 7)) history ((tagA (0 7 1 0 0 1 4 0)) (repA (0 0)) (orbitA (() () () () () (4 3) () ())))
groupCap ((0 7) (0 7) (6 7)) history ((tagA (0 7 1 0 0 1 6 0)) (repA (0 0)) (orbitA (() () () () () (6 4 3) () ())))
groupCap ((0 7) (0 7) (6 7)) history ((tagA (0 7 1 0 0 1 6 0)) (repA (0 0)) (orbitA (() () () () () (6 2) () ())))
groupCap ((0 7) (0 7) (5 7)) history ((tagA (0 7 1 0 0 1 5 0)) (repA (0 0)) (orbitA (() () () () () (5 2) () ())))
groupCap ((0 7) (0 7) (4 7)) history ((tagA (0 7 1 0 0 1 4 0)) (repA (0 0)) (orbitA (() () () () () (4 2) () ())))
groupCap ((0 7) (0 7) (6 7)) history ((tagA (0 7 1 0 0 1 6 0)) (repA (0 0)) (orbitA (() () () () () (6 4 2) () ())))
groupCap ((0 7) (0 7) (4 7)) history ((tagA (0 7 1 0 0 1 4 0)) (repA (0 0)) (orbitA (() () () () () (4 1) () ())))
groupCap ((0 7) (0 7) (6 7)) history ((tagA (0 7 1 0 0 1 6 0)) (repA (0 0)) (orbitA (() () () () () (6 4 1) () ())))
groupCap ((0 7) (6 7) (6 7)) history ((tagA (0 7 1 6 0 1 6 0)) (repA (0 0)) (orbitA (() () (6) () () () () ())))
groupCap ((0 7) (6 7) (6 7)) history ((tagA (0 7 1 6 0 1 6 0)) (repA (0 0)) (orbitA (() () (6) () () () () ())))
groupCap ((0 7) (6 7) (6 7)) history ((tagA (0 7 1 6 0 1 6 0)) (repA (0 0)) (orbitA (() () (6) () () () () ())))
groupCap ((0 7) (6 7) (6 7)) history ((tagA (0 7 1 6 0 1 6 0)) (repA (0 0)) (orbitA (() () (6) () () () () ())))
groupCap ((0 7) (5 7) (5 7)) history ((tagA (0 7 1 5 0 1 5 0)) (repA (0 0)) (orbitA (() () (5) () () () () ())))
groupCap ((0 7) (5 7) (5 7)) history ((tagA (0 7 1 5 0 1 5 0)) (repA (0 0)) (orbitA (() () (5) () () () () ())))
groupCap ((0 7) (4 7) (4 7)) history ((tagA (0 7 1 4 0 1 4 0)) (repA (0 0)) (orbitA (() () (4) () () () () ())))
groupCap ((0 7) (4 7) (4 7)) history ((tagA (0 7 1 4 0 1 4 0)) (repA (0 0)) (orbitA (() () (4) () () () () ())))
groupCap ((0 7) (4 7) (4 7)) history ((tagA (0 7 1 4 0 1 4 0)) (repA (0 0)) (orbitA (() () (4) () () () () ())))
groupCap ((0 7) (4 7) (6 7)) history ((tagA (0 7 1 4 0 1 6 0)) (repA (0 0)) (orbitA (() () (4) () () (6) () ())))
groupCap ((0 7) (4 7) (6 7)) history ((tagA (0 7 1 4 0 1 6 0)) (repA (0 0)) (orbitA (() () (4) () () (6) () ())))
groupCap ((0 7) (4 7) (6 7)) history ((tagA (0 7 1 4 0 1 6 0)) (repA (0 0)) (orbitA (() () (4) () () (6) () ())))
groupCap ((0 7) (6 7) (6 7)) history ((tagA (0 7 1 6 0 1 6 0)) (repA (0 0)) (orbitA (() () (6 4) () () () () ())))
groupCap ((0 7) (6 7) (6 7)) history ((tagA (0 7 1 6 0 1 6 0)) (repA (0 0)) (orbitA (() () (6 4) () () () () ())))
groupCap ((0 7) (6 7) (6 7)) history ((tagA (0 7 1 6 0 1 6 0)) (repA (0 0)) (orbitA (() () (6 4) () () () () ())))
groupCap ((0 7) (3 7) (5 7)) history ((tagA (0 7 1 3 0 1 5 0)) (repA (0 0)) (orbitA (() () (3) () () (5) () ())))
groupCap ((0 7) (3 7) (4 7)) history ((tagA (0 7 1 3 0 1 4 0)) (repA (0 0)) (orbitA (() () (3) () () (4) () ())))
groupCap ((0 7) (3 7) (6 7)) history ((tagA (0 7 1 3 0 1 6 0)) (repA (0 0)) (orbitA (() () (3) () () (6 4) () ())))
groupCap ((0 7) (6 7) (6 7)) history ((tagA (0 7 1 6 0 1 6 0)) (repA (0 0)) (orbitA (() () (6 3) () () () () ())))
groupCap ((0 7) (5 7) (5 7)) history ((tagA (0 7 1 5 0 1 5 0)) (repA (0 0)) (orbitA (() () (5 3) () () () () ())))
groupCap ((0 7) (4 7) (4 7)) history ((tagA (0 7 1 4 0 1 4 0)) (repA (0 0)) (orbitA (() () (4 3) () () () () ())))
groupCap ((0 7) (4 7) (6 7)) history ((tagA (0 7 1 4 0 1 6 0)) (repA (0 0)) (orbitA (() () (4 3) () () (6) () ())))
groupCap ((0 7) (6 7) (6 7)) history ((tagA (0 7 1 6 0 1 6 0)) (repA (0 0)) (orbitA (() () (6 4 3) () () () () ())))
groupCap ((0 7) (2 7) (6 7)) history ((tagA (0 7 1 2 0 1 6 0)) (repA (0 0)) (orbitA (() () (2) () () (6) () ())))
groupCap ((0 7) (2 7) (5 7)) history ((tagA (0 7 1 2 0 1 5 0)) (repA (0 0)) (orbitA (() () (2) () () (5) () ())))
groupCap ((0 7) (2 7) (4 7)) history ((tagA (0 7 1 2 0 1 4 0)) (repA (0 0)) (orbitA (() () (2) () () (4) () ())))
groupCap ((0 7) (2 7) (6 7)) history ((tagA (0 7 1 2 0 1 6 0)) (repA (0 0)) (orbitA (() () (2) () () (6 4) () ())))
groupCap ((0 7) (6 7) (6 7)) history ((tagA (0 7 1 6 0 1 6 0)) (repA (0 0)) (orbitA (() () (6 2) () () () () ())))
groupCap ((0 7) (6 7) (6 7)) history ((tagA (0 7 1 6 0 1 6 0)) (repA (0 0)) (orbitA (() () (6 2) () () () () ())))
groupCap ((0 7) (5 7) (5 7)) history ((tagA (0 7 1 5 0 1 5 0)) (repA (0 0)) (orbitA (() () (5 2) () () () () ())))
groupCap ((0 7) (4 7) (4 7)) history ((tagA (0 7 1 4 0 1 4 0)) (repA (0 0)) (orbitA (() () (4 2) () () () () ())))
groupCap ((0 7) (4 7) (6 7)) history ((tagA (0 7 1 4 0 1 6 0)) (repA (0 0)) (orbitA (() () (4 2) () () (6) () ())))
groupCap ((0 7) (6 7) (6 7)) history ((tagA (0 7 1 6 0 1 6 0)) (repA (0 0)) (orbitA (() () (6 4 2) () () () () ())))
groupCap ((0 7) (1 7) (4 7)) history ((tagA (0 7 1 1 0 1 4 0)) (repA (0 0)) (orbitA (() () (1) () () (4) () ())))
groupCap ((0 7) (1 7) (6 7)) history ((tagA (0 7 1 1 0 1 6 0)) (repA (0 0)) (orbitA (() () (1) () () (6 4) () ())))
groupCap ((0 7) (6 7) (6 7)) history ((tagA (0 7 1 6 0 1 6 0)) (repA (0 0)) (orbitA (() () (6 1) () () () () ())))
groupCap ((0 7) (4 7) (4 7)) history ((tagA (0 7 1 4 0 1 4 0)) (repA (0 0)) (orbitA (() () (4 1) () () () () ())))
groupCap ((0 7) (4 7) (6 7)) history ((tagA (0 7 1 4 0 1 6 0)) (repA (0 0)) (orbitA (() () (4 1) () () (6) () ())))
groupCap ((0 7) (6 7) (6 7)) history ((tagA (0 7 1 6 0 1 6 0)) (repA (0 0)) (orbitA (() () (6 4 1) () () () () ())))

--  --  --  --  --

--  --  --  --  --

  *)
