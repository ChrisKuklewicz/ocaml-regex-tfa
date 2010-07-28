(* CorePattern.ml starting as transliteration of CorePattern.hs *)
(* Much of the construction of CorePattern in Haskell depended on laziness.
   Learn enough of OCaml's lazy pattern matching to see if it is still possible.
   Otherwise change some or all names field to be mutable.

   First pass : convert pattern to corePattern gathering length information
   Second pass : mutate corePattern, removing CaptureGroup nodes and setting tag information

*)
open CamomileLibrary
open Pattern
open ReadPattern
open WhichTest
open Common

TYPE_CONV_PATH "CorePattern"

type uset = USet.t

let sexp_of_uset u = Sexplib.Conv.sexp_of_list (Sexplib.Conv.sexp_of_pair sexp_of_uchar sexp_of_uchar) (USet.ranges u)

let uset_of_sexp s = List.fold_left (fun x (lo,hi) -> USet.add_range lo hi x) USet.empty (Sexplib.Conv.list_of_sexp (Sexplib.Conv.pair_of_sexp uchar_of_sexp uchar_of_sexp) s)

let all_unicode = USet.add_range (UChar.of_int 0) (UChar.of_int 0x10ffff) USet.empty

(* utility *)

let const a = fun _ -> a

(* the first n elements of xsIn are prepended in reverse order onto the front of ending *)
let take_append n xsIn ending =
  let rec go acc i xs = match (i,xs) with
      (0,_) -> acc
    | (_,[]) -> acc
    | (_,(y::ys)) -> y :: go (y::acc) (i-1) ys
  in if n <= 0 then ending else go ending n xsIn

let liftOpt f a b = match (a,b) with
    (None,_) -> None
  | (_,None) -> None
  | (Some a,Some b) -> Some (f a b)

let pureRepeatOnto ending item = 
  let rec go n = if n < 1 then ending else item :: go (n-1)
  in go

let thunkRepeatOnto ending thunk = 
  let rec go n = if n < 1 then ending else let me = thunk () in me :: go (n-1)
  in go

(* Possible sub-module for handleTag *)

type handleTag = NoTag            (* No tag at this boundary *)
                 | Advice of tag  (* tag at this boundary, applied at higher level in tree *)
                 | Apply of tag   (* tag at this boundary, may be applied at this node or passed to one child *)
with sexp

let seeht ht = Sexplib.Sexp.to_string_hum (sexp_of_handleTag ht)

let apply       = function | Apply tag -> Some tag                  | _ -> None
let asAdvice    = function | Apply tag -> Advice tag                | s -> s
let toPreUpdate = function | Apply tag -> [(tag,PreUpdate TagTask)] | _ -> []

(* Experiment with replacing Test of whichTest with Test of testSet *)
(* Experiment with replacing Empty constructor with (Test AlwaysTrue) *)
(* Experiment with making OneChar hold a USet *)
(* Experiment with Bound low,hi instead of Star *)
type corePattern =
    Or of coreQ list
  | Seq of coreQ*coreQ
  | Repeat of repeatQ
  | Test of testSet
  | OneChar of uset*patIndex
  | CaptureGroup of capGroupQ

and capGroupQ = { parentGroup : groupIndex     (* from pattern PGroup's parentGI *)
                ; myGroup : groupIndex         (* from pattern PGroup's thisGI *)
                ; mutable preReset : tag list  (* flows up "via writer"? in second pass *)
                ; mutable postSet : tag        (* flows up *)
                ; subPat : coreQ               (* NOTE: this is not coreQ *) 
                }

(* repeatQ is still being designed *)
and repeatQ = { lowBound : int            (* flows up *)
              ; optHiBound : int option   (* flows up *)
              ; repDepth : int            (* flows down in state of first pass *)
              ; needsOrbit : bool         (* flows up *)
              ; mutable getOrbit : tag option (* flows down in second pass *)
              ; mutable resetOrbits : tag list (* flows down in second pass *)
              ; unRep : coreQ
              }

and coreQ = { takes : int * int option  (* flows up for use in first pass only *)
            ; childGroups : bool        (* flows up for use in first pass only *)
            ; tagged : bool             (* flows up *)
            ; wants : wanted            (* flows up *)
            ; mutable preTag : tag option  (* flows down in second pass *)
            ; mutable postTag : tag option (* flows down in second pass *)
            ; mutable nullQ : nullView  (* flows up in second pass after down in second pass *)
            ; unQ : corePattern         (* must be mutable to remove CaptureGroup from corePattern *)
            }
and corePatternZip = 
    CorePatternTop
  | Or'Zip
  | Seq1'Zip
  | Seq2'Zip
  | Repeat'Zip
  | CaptureGroup'Zip
and corePatternLoc = coreQ * corePatternZip
with sexp

(* nothing is the blank template pattern *)
(* epsilon is the corePattern that repesents an empty always accepting pattern *)
let nothing = { takes = (0,Some 0)
              ; childGroups = false
              ; tagged = false
              ; wants = WantsEither
              ; preTag = None
              ; postTag = None
              ; nullQ = []
              ; unQ = Test AlwaysFalse (* This should always be overridden *)
              }
let epsilon = { nothing with unQ = Test AlwaysTrue }

type groupInfo = { parentIndex : groupIndex
                 ; thisIndex : groupIndex
                 ; startTag : tag
                 ; stopTag : tag
                 ; flagTag : tag
                 }
with sexp

type coreResult = { cp : coreQ
                  ; tags : tagOP array
                  ; groups : groupInfo array
                  ; depth : int
                  }
with sexp

let varies = function
  | (_,None) -> true
  | (x,Some y) -> x<>y

let toUSet bs =
  let addIt s = function
    | BPChar c -> USet.add (getBC c) s
    | BPRange (lo,hi) -> USet.add_range (getBC lo) (getBC hi) s
    | BPSet (BClassElem name) -> 
      let u x = UChar.of_char x in
      let chr = Char.chr in
      let addRanges xs = List.fold_left (fun x (lo,hi) -> USet.add_range (u lo) (u hi) x) s xs in
      let ranges = match name with
          "alnum" -> [('0','9');('a','z');('A','Z')]
        | "digit" -> [('0','9')]
        | "punct" -> [(chr 33,chr 47);(chr 58, chr 64);(chr 91,chr 96);(chr 123,chr 126)]
        | "alpha" -> [('a','z');('A','Z')]
        | "graph" -> [(chr 41,chr 126)]
        | "space" -> [(chr 9,chr 13);(' ',' ')]
        | "blank" -> [(chr 9,chr 9);(' ',' ')]
        | "lower" -> [('a','z')]
        | "upper" -> [('A','Z')]
        | "cntrl" -> [(chr 0,chr 31);(chr 127,chr 127)]
        | "print" -> [(chr 32,chr 126)]
        | "xdigit"-> [('0','9');('a','f');('A','F')]
        | "word"  -> [('0','9');('a','z');('A','Z');('_','_')]
        | _ -> failwith "invalid character class"
      in addRanges ranges
    | BPSet (BEquivClassElem c) -> USet.add c s
  in List.fold_left addIt USet.empty bs

let cannotTake = function q -> match q.takes with
    (_,Some 0) -> true
  | _ -> false

(* This concatenation is not very efficient yet, and resetGroupTags starts with the same tag value
   as setGroupTag which is thus redundant to reset *)
let addGroupResetsToNullView resetGroupTags setGroupTag nvs =
  let resetSome = List.map (fun tag -> (tag,PreUpdate ResetGroupStopTask)) resetGroupTags
  and setOne = [(setGroupTag,PreUpdate SetGroupStopTask)]
  in let updatePair (test,(tags,reps)) = (test,(resetSome @ tags @ setOne,reps))
     in List.map updatePair nvs

(* TODO: go through and use rev_map or something to make this more efficient *)
let rec cleanNullView = function
  | [] -> []
  | ((AlwaysTrue,_) as first :: rest) -> first :: []
  | ((testSet,_) as first :: rest) -> 
    let notDominated (testSet2,_) = not (dominates testSet testSet2)
    in first :: cleanNullView (List.filter notDominated rest)

(* TODO: go through and use rev_map or something to make this more efficient *)
let seqNullViews s1 s2 =
  let overS1 (test1,(tags1,reps1)) = 
    let overS2 (test2,(tags2,reps2)) = 
      (WhichTestMonoid.mappend test1 test2, (tags1 @ tags2,reps1 @ reps2))
    in List.map overS2 s2
  in cleanNullView (List.concat (List.map overS1 s1))

let tagWrapNullView ha hb oldNV =
  match (toPreUpdate ha,toPreUpdate hb) with
      ([],[]) -> oldNV
    | (pre,post) -> let updatePair (oldTests,(oldTags,reps)) = (oldTests,(pre @ oldTags @ post,reps))
                    in List.map updatePair oldNV

(* Four cases for list handling efficiency *)
let orbitWrapNullView r optOrbit orbitResets oldNV =
  let preROT = List.rev_map (fun t -> (t,PreUpdate ResetOrbitTask)) orbitResets in
  let (pre,post) = match optOrbit with
      None -> (preROT,[])
    | Some o -> ((o,PreUpdate EnterOrbitTask) :: preROT,[(o,PreUpdate LeaveOrbitTask)])
  and topCount = match r.optHiBound with
      None -> 1+r.lowBound
    | Some hi -> hi
  in
  let preRep = (r.repDepth,PreUpdate (IncRep topCount))
  and postRep = (r.repDepth,PreUpdate LeaveRep) in
  let updatePair (oldTests,(oldTags,oldReps)) = (oldTests,
                                                 (List.rev_append pre (oldTags @ post),
                                                 preRep :: (oldReps @ [postRep])))
  in
  List.map updatePair oldNV

type appliedBoth = AppliedBoth (* Used instead of () : unit to ensure applyBoth is called in each branch *)

let toCorePattern (patternIn) : coreResult =
  (* combineOr is the only place where Or nodes are constructed *)
  let combineOr firstChild secondChild restChildren =
    let children = firstChild::secondChild::restChildren in
    let lo = List.fold_left (fun t q -> min t (fst q.takes)) (fst firstChild.takes) (secondChild::restChildren)
    and hi = List.fold_left (fun t q -> liftOpt max t (snd q.takes)) (snd firstChild.takes) (secondChild::restChildren)
    and anyChildGroups = List.exists (fun q -> q.childGroups) children
    in { nothing
         with takes = (lo,hi)
           ; childGroups = anyChildGroups
           ; tagged = varies (lo,hi) || anyChildGroups
           ; wants =  if List.exists (fun q -> WantsState=q.wants) children then WantsState
             else if List.exists (fun q -> WantsBundle=q.wants) children then WantsBundle
             else WantsEither
           ; unQ = Or children
       }
  (* combineSeq is the only place where Seq nodes are constructed *)
  and combineSeq qFront qEnd =
    { nothing
      with takes = (fst qFront.takes + fst qEnd.takes
                      ,liftOpt (+) (snd qFront.takes) (snd qEnd.takes))
        ; childGroups = qFront.childGroups || qEnd.childGroups
        ; tagged = varies qFront.takes && varies qEnd.takes
        ; wants = if WantsEither=qEnd.wants then qFront.wants else qEnd.wants
        ; unQ = Seq (qFront,qEnd)
    }
  in
  let rec repDepthRef = ref 0 (* simple enough to avoid using an OCaml object *)
  (* Note: repDepthRef is not spoiled by order of execution of branches or pieces *)
  (* XXX TODO : record max repDepth when running ? *)
  (* DEFINE THE FIRST PASS CONVERSION FROM pattern TO corePattern *)
  and doPattern p =
    match p with
        [] ->  epsilon (* either fully empty pattern or open-close parenthesis pair () *)
      | [b] -> doBranch b
      | (b1::b2::bs) -> combineOr (doBranch b1) (doBranch b2) (List.map doBranch bs)
  and doBranch (b,bs) =
    match bs with
        [] -> doElemAt b
      | _ ->(* The "rev" below means fold_left is used below instead of fold_right *)
        match List.rev_map doElemAt (b::bs) with
            [] -> failwith "impossible corePattern doBranch"
          | (lastChild::revChildren) -> List.fold_left
            (fun qEnd qFront -> combineSeq qFront qEnd)
            lastChild revChildren
  and doElemAt (e,i) =
    match e with
        PAtom a -> doAtom a i
      | PAnchor a -> doAnchor a i
      | PRepeat (a,r) -> 
        match r with
            PQuest -> doElemAt (PRepeat (a,PBound (0,Some 1)),i)
          | PPlus -> doElemAt (PRepeat (a,PBound (1,None)),i)
          | PStar -> doElemAt (PRepeat (a,PBound (0,None)),i)
          | PBound (badI,_) when badI < 0 -> failwith (Printf.sprintf "invalid bound repetition {%i,_} at byte %i" badI i)
          | PBound (badI,Some badJ) when badI > badJ -> failwith (Printf.sprintf "invalid bound repetion {%i,%i} at byte %i" badI badJ i)
          | PBound (0,Some 0) -> epsilon
          | PBound (0,Some 1) -> combineOr (doAtom a i) epsilon []
          | PBound (1,Some 1) -> doElemAt (PAtom a,i)
          | PBound (i,optJ) -> 
            (* Only increment and use repDepth when there may be more than 1 repetition *)
            let myDepth = !repDepthRef in
            repDepthRef := myDepth+1;
            let q = doAtom a i in
            repDepthRef := myDepth;
            if cannotTake q then 
              (* Since q cannot accept characters the myDepth increment had no effect during (doAtom a i) *)
              if i==0 then combineOr q epsilon [] else q
            else
              let lo = i*(fst q.takes)
              and hi = liftOpt ( * ) optJ (snd q.takes) (* assert : neither j nor k can be Some 0 here *)
              and needsOrbit = varies q.takes && q.childGroups in
              let needsTags = needsOrbit || (not (cannotTake q)) in
              { nothing
                with takes = (lo,hi)
                  ; childGroups = q.childGroups
                  ; tagged = needsTags
                  ; wants = WantsBundle
                  ; unQ = Repeat { lowBound = i
                                 ; optHiBound = optJ
                                 ; repDepth = myDepth
                                 ; needsOrbit = needsOrbit
                                 ; getOrbit = None  (* set below in addTags *)
                                 ; resetOrbits = [] (* set below in addTags *)
                                 ; unRep = q }
              }
  and doAtom atom i =
    let one s = { nothing
                  with takes = (1,Some 1)
                    ; wants = WantsState
                    ; unQ = OneChar(s,i)
                }
    in match atom with
        PDot -> one all_unicode
      | PEscape c -> one (USet.singleton c)
      | PChar c -> one (USet.singleton c)
      | PBracket (false,bs) -> one (toUSet bs)
      | PBracket (true,bs) -> one (USet.diff all_unicode (toUSet bs))
      | PGroup {parentGI=myParent;thisGI=myGI;subPattern=p} -> 
        let q = doPattern p in
        { q
          with childGroups = true
            ; tagged = true
            (* leave q.tagged alone as CaptureGroups sends same signal below *)
            ; unQ = CaptureGroup { parentGroup = myParent
                                 ; myGroup = myGI
                                 ; preReset = []  (* set below in addTags *)
                                 ; postSet = (-1) (* set below in addTags *)
                                 ; subPat = q }
        }
  and doAnchor anchor i =
    let test wt = { nothing
                    with wants = WantsBundle
                      ; unQ = Test (singleTest (true,wt,i))
                  }
    in match anchor with
        PCarat -> test Test_BOL
      | PDollar -> test Test_EOL
  in
  (* TODO: consider changing tag state into an OCaml object *)
  let rec nextTagRef = ref 2
  and tagOPsLog = ref [Maximize;Minimize] (* in reverse order, ends with tag 0 *)
  and uniq' msg op = let tag = !nextTagRef in
                 begin
                   nextTagRef := tag+1;
                   tagOPsLog := op :: !tagOPsLog;
                   Printf.printf "%s = %i\n" msg tag; 
                   tag
                 end
  and uniq msg = Apply (uniq' msg Maximize)
  and groupFlag () = uniq' "groupFlag" GroupFlag
  and getOrMake = function 
    | NoTag -> let t = uniq' "getOrMake" Maximize in (t,Apply t)
    | (Advice t as ht) | (Apply t as ht) -> (t,ht)
  (* TODO: consider changing orbit state into an OCaml object *)
  and orbitInfoLog = ref []
  and makeOrbit () =
    let tag = uniq' "makeOrbit" Orbit in
    orbitInfoLog := tag :: !orbitInfoLog;
    tag
  and withOrbit lazyThunk =
    let origLength = List.length !orbitInfoLog in
    let value = Lazy.force lazyThunk in
    let newLength = List.length !orbitInfoLog in
    let orbitTags = take_append (newLength-origLength) !orbitInfoLog [] in
    (value,orbitTags)
  (* TODO: consider changing group info state into an OCaml object *)
  and groupInfoRef = ref [ [] ] (* Used  *)
  and groupInfoLog = ref []     (* Used to accumulate all knowledge of groupInfo *)
  and pushGroup () = groupInfoRef := [] :: !groupInfoRef
  and popGroup () = match !groupInfoRef with
      [] -> failwith "impossible corePattern popGroup"
    | (siblings :: ancestors) -> groupInfoRef := ancestors; siblings
  and listenGroups lazyThunk =
    begin
      pushGroup ();
      let value = Lazy.force lazyThunk in
      let children = popGroup () in
      (value,children)
    end
  and makeGroup groupInfo = match !groupInfoRef with
      [] -> failwith "impossible corePattern makeGroup"
    | (siblings :: ancestors) -> ( groupInfoRef := (groupInfo.flagTag :: siblings) :: ancestors
                                 ; groupInfoLog := groupInfo :: !groupInfoLog )
  (* DEFINE THE SECOND PASS MUTATION UPDATE OF corePattern *)
  and addTags q m1 m2 : appliedBoth =
    let acquire () = let ha = if q.tagged && (m1=NoTag) then uniq "acquire preTag" else m1 in
                     let hb = if q.tagged && (m2=NoTag) then uniq "acquire postTag" else m2 in
                     (ha,hb)
    and applyBoth a b newNullView =
      q.preTag <- apply a;
      q.postTag <- apply b;
      q.nullQ <- tagWrapNullView a b newNullView;
      AppliedBoth
    in
    match q.unQ with
        OneChar _ -> applyBoth m1 m2 []
      | Test testSet -> applyBoth m1 m2  [(testSet,([],[]))]
      | CaptureGroup cg ->
        begin
          let (a,ha) = getOrMake m1 in let (b,hb) = getOrMake m2 in
          let flag = groupFlag () in
          makeGroup { parentIndex = cg.parentGroup; thisIndex = cg.myGroup
                    ; startTag = a; stopTag = b
                    ; flagTag = flag };
          (* Note: makeGroup is done before addTags on child *)
          let (AppliedBoth,childFlags) = listenGroups (lazy (addTags cg.subPat ha hb)) in
          let resetGroupTags = flag::childFlags in
          let nullView = addGroupResetsToNullView resetGroupTags flag cg.subPat.nullQ in
          (* forcing cg.subPat.nullQ to [] since it cannot accept 0 characters itself ? *)
          cg.subPat.nullQ <- [];
          cg.preReset <- resetGroupTags;
          cg.postSet <- flag;
          applyBoth NoTag NoTag nullView
        end
      | Seq (qFront,qEnd) ->
        begin
          let (ha,hb) = acquire () in
          let mid = match ( ha<>NoTag && cannotTake qFront
                          , hb<>NoTag && cannotTake qEnd
                          , qFront.tagged || qEnd.tagged
                          ) with
              (true,_,_) -> asAdvice ha 
            | (_,true,_) -> asAdvice hb
            | (_,_,true) -> uniq "seq mid tag"
            | _ -> NoTag
          in
          ignore (addTags qFront ha mid);
          ignore (addTags qEnd (asAdvice mid) hb);
          applyBoth NoTag NoTag (seqNullViews qFront.nullQ qEnd.nullQ)
        end
      | Or qs -> (* qs will have length of at least two *)
        begin
          let (ha,hb) = acquire () in
          let aAdvice = asAdvice ha and bAdvice = asAdvice hb in
          let bs = if q.tagged
            then thunkRepeatOnto [bAdvice] (fun () -> uniq "or thunk") (List.length qs-1)
            else pureRepeatOnto [] bAdvice (List.length qs)
          in
          (*Printf.printf "or (%s,%s)\n" (seeht ha) (seeht hb);List.iter (fun bTag -> Printf.printf "or b %s\n" (seeht bTag)) bs; *)
          List.iter2 (fun branch bTag -> ignore (addTags branch aAdvice bTag)) qs bs;
          let nullView = cleanNullView (List.concat (List.map (fun q -> q.nullQ) qs)) in
          (* forcing r.unRep.nullQ to [] since it cannot accept 0 characters itself ? *)
          List.iter (fun q -> q.nullQ <- []) qs;
          applyBoth ha hb nullView
        end
      | Repeat r -> 
        let (ha,hb) = acquire () in
        let optOrbit = if r.needsOrbit then Some (makeOrbit ()) else None in
        let (AppliedBoth,resetOrbitTags) = withOrbit (lazy (addTags r.unRep NoTag NoTag)) in
        (* A value in optOrbit is never included in resetOrbitTags *)
        let nullView =
          let childView =
            if 0 < r.lowBound then r.unRep.nullQ
            else cleanNullView ( r.unRep.nullQ @ [(AlwaysTrue,([],[]))] )
          in orbitWrapNullView r optOrbit resetOrbitTags childView
        in
        (* forcing r.unRep.nullQ to [] since it cannot accept 0 characters itself ? *)
        r.unRep.nullQ <- [];
        r.getOrbit <- optOrbit;
        r.resetOrbits <- resetOrbitTags;
        applyBoth ha hb nullView
  in
  let coreP = doPattern patternIn in
  (* let s = Sexplib.Sexp.to_string_hum (sexp_of_coreQ coreP) in Printf.printf "%s\n" s; *)
  let AppliedBoth = addTags coreP (Advice 0) (Advice 1) in
  { cp = coreP
  ; tags = Array.of_list (List.rev !tagOPsLog)
  ; groups = Array.of_list (List.rev !groupInfoLog)
  ; depth = !repDepthRef
  }

type ('a , 'b) either = Left of 'a | Right of 'b

let kick s = let pe = parseRegex s in
             let cp = match pe with
                     ParseFail err -> Left err
                   | ParseSucceed p -> Right (p,toCorePattern p)
             in begin
               Printf.printf "OUT: %s\n" (see pe);
               match cp with
                   Right (p1,{cp=p2;tags=at;groups=ag}) -> 
                     let s = Sexplib.Sexp.to_string_hum (Sexplib.Conv.sexp_of_array sexp_of_tagOP at) in Printf.printf "tagOP %s\n" s;
                     let s = Sexplib.Sexp.to_string_hum (Sexplib.Conv.sexp_of_array sexp_of_groupInfo ag) in Printf.printf "groupInfo %s\n" s;
                     let s = Sexplib.Sexp.to_string_hum (sexp_of_pattern p1) in Printf.printf "%s\n" s;
                     let s = Sexplib.Sexp.to_string_hum (sexp_of_coreQ p2) in Printf.printf "%s\n" s;
                 | _ -> ();
             end 

let test () =
  begin
    kick "a(b|cd)*e";
    kick "a\xCE\xA9|^^|^^^";
    kick "a|bc|def";
    kick "(a|bc|def)";
    kick "((a|bc|def))";
    kick "(a|bc)|(gh|ij)";
    kick "";
    kick "()";
    kick "(ab)?(c+d*)[efg]{4,5}";
    kick "()|(1(a|b))";
    kick ".|a{b{|{(\\|){";
    kick "a{0123}";
    kick "a{0123,123}";
    kick "a{0123,}";
    kick "a{0123,456}";
    kick "a{0123,0123}";
    kick "a{1}";
    kick "a{1,}";
    kick "a{1,2}";
    kick "(a(b|cd){5,6}e){3,}";
    kick "[]]";
    kick "[^]-]";
    kick "[]x\xCE\xA9]";
    kick "[]x\\]";
    kick "[0-9]";
    kick "[^0-9-]";
    kick "[-0-9]";
    kick "[0-9a-z]";
    kick "[-]";
    kick "[--]";
    kick "[&--A]";
    kick "[&[.-.]-A]";
    kick "[ds;fklja;ofr;owean;oiweunr;oiwuenr;oxiuo;iycwiuyr274p26cp416p3123nxiuliu378zbo8o897bx3p98127n3p98zx32 3n4pc9824897n]";
    kick "[1-2[:alpha:]3-4]";
    kick "[02-[.a.]c]";
    kick "[&[.-.]-mq]";
    kick "[1-2[.a.]3-4]";
    kick "[1-2[=a=]3-4]";
    kick "[1-[.a.]3]";
    kick "[1,--3]";
    kick "[1+--3]";
    kick "[[.].]-a]";
    kick "[A-[.].]]";
    kick "[1[.a.]-[.b.]3]";
    kick "[1[.a.]-[.d.]3]";
  end
