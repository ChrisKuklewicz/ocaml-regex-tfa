open Sexplib.Std
open CamomileLibrary
open Pattern
open Common
open Core.Result

TYPE_CONV_PATH "ReadPattern"

module UTF8Set = Set.Make(UTF8)
type utf8Set = UTF8Set.t

type uchar = CamomileLibrary.UChar.t

type ustring = UTF8.t;;

(* external type describing the result of a parse attempt *)
type parseEnd = (pattern,string) Core.Result.t
(* ParseFail of string | ParseSucceed of pattern*)

type parseF = (* parseF is only used internally *)
    ParseWith of (int * uchar-> parseF) * (unit -> parseEnd)
  | ParseError of string

let see pe = match pe with Error e -> e; | Ok p -> show_pattern p

(* My unicocde UTF8 hacks until I learn Camomile better *)
(* u injects ascii char to unicode char *)
let u = UChar.of_char
(* e is used to encode unicode char to string holding utf8 *)
let e c = let b = UTF8.Buf.create 4 in
          UTF8.Buf.add_char b c;
          UTF8.Buf.contents b;;
(* ee is used to encode list of unicode char to string holding utf8 *)
let ee cs = let b = UTF8.Buf.create (List.length cs) in
            List.iter (UTF8.Buf.add_char b) cs;
            UTF8.Buf.contents b;;

let pr = Printf.printf;;
let spr = Printf.sprintf;;

(* This walks the utf8 encoded string and feeds it to the parser in a single pass *)
let iterParse : parseF -> ustring -> parseEnd = fun pf0 s ->
  let byteLength = String.length s in
  let rec go bytePos p = 
    match p with
      ParseError message -> Error (spr "At %d: %s" (bytePos-1) message)
    | ParseWith (step,_) when bytePos < byteLength -> go (UTF8.next s bytePos) (let c = UTF8.look s bytePos in step (bytePos,c))
    | ParseWith (_,stop) -> stop ()
  in go (UTF8.first s) pf0

let toUSet m = List.fold_left (fun a b -> USet.add (u b) a) USet.empty m

(* Sigh. Learn to do a left fold over strings at some point *)
let specials = toUSet ['^';'$';'.';'*';'+';'?';'(';'[';')';'{';'\\';'|']
let digits = USet.add_range (u '0') (u '9') USet.empty
let allowedClasses = List.fold_left (fun a b -> UTF8Set.add b a) UTF8Set.empty 
     ["alpha";"graph";"space";"blank";"lower";"upper";"cntrl";"print";"xdigit"]

let unexpected c = ParseError (spr "Unexpected character %s" (e c))

type parseEnv = { parentGroupIndex : groupIndex
                ; nextGroupIndexRef : groupIndex ref
                ; popCont : (pattern -> parseF) option }

(* pushBranch holds a hand written DFA-ish parser for POSIX extended regular expressions *)
(* When not at the top-level (in a parenthesied group) pop holds the continuation to complete this level *)
(* Establish lexical context with "branches" which is built in reverse order *)
let rec pushBranch env branches (iPB,cPB) =
  let rec nextPiece pieces (iNP,cNP) = (* "pieces" is built in reverse order *)
    let accept p = let data = (p :: pieces) in ParseWith (nextPiece data,endPiece data) in
    let newGroup ()= let gi = !(env.nextGroupIndexRef) in
                     env.nextGroupIndexRef := gi+1;
                     let newCont subPattern = accept (PAtom (PGroup {parentGI=env.parentGroupIndex
                                                                    ;thisGI=gi
                                                                    ;subPattern=subPattern})
                                                            ,iNP) in
                     let newEnv = {env with parentGroupIndex=gi; popCont = Some newCont} in
                     ParseWith (pushBranch newEnv [],fun () -> Error "expected right parenthesis")
    in
    if not (USet.mem cNP specials) then accept (PAtom (PChar cNP),iNP) else
    match UChar.char_of cNP with
        '^' -> accept (PAnchor PCarat,iNP)
      | '$' -> accept (PAnchor PDollar,iNP)
      | '.' -> accept (PAtom PDot,iNP)
      | '*' -> post pieces cNP PStar
      | '+' -> post pieces cNP PPlus
      | '?' -> post pieces cNP PQuest
      | '\\' -> ParseWith ((fun (_,a) -> accept (PAtom (PEscape a),iNP))
                          ,fun () -> Error "expected escaped character")
      | '(' -> newGroup ()
      | '{' -> ParseWith (leftBrace pieces
                         ,endPiece ((PAtom (PChar cNP),iNP)::pieces))
      | '[' -> ParseWith (leftBracket pieces
                         ,fun () -> Error "expected right bracket")
      | '|' ->
        begin
          match (List.rev pieces) with
              [] -> ParseError (spr "Unexpected character %s, empty branch not allowed" (e cNP))
            | (p::ps) -> ParseWith (pushBranch env ((p,ps)::branches),fun () -> Error "emptyBranch")
        end
      | ')' -> (match env.popCont with
            None -> unexpected cNP
          | Some cont -> (match (List.rev pieces) with
                [] -> if branches=[]
                  then cont [] (* parse the open-close parenthesis pair () *)
                  else ParseError (spr "Unexpected character %s, empty branch not allowed" (e cNP))
              | (p::ps) -> cont (List.rev ((p,ps)::branches))
          ))
(*
      | ')' -> (match env.popCont with
            None -> unexpected cNP
          | Some cont -> (match (List.rev pieces) with
                [] -> ParseError (spr "Unexpected character %s, empty branch not allowed" (e cNP))
              | (p::ps) -> cont (List.rev ((p,ps)::branches))
          ))
*)
      | imp -> failwith (spr "Impossible character (%c) in match statement when at position %i in regular expression" imp iNP)
  and post pieces cP rep = match pieces with
      ((PAtom a,i)::xs) -> let data = ((PRepeat (a,rep),i) :: xs) in ParseWith (nextPiece data,endPiece data)
    | ((PAnchor _,_)::_) -> ParseError "Cannot apply a postfix repetition operator to an anchor"
    | ((PRepeat _,_)::_) -> ParseError "Cannot appply more than one postfix repetition operator in a row"
    | [] -> ParseError "Nothing to repeat"
  (* A left brace followed by a digit must be a repetion operator (firstBound) *)
  (* A left brace not followed by a digit matches a '{' character (jump to nextPiece) *)
  (* Establish lexical context with "pieces" which is built in reverse order *)
  and leftBrace pieces (iLB,cLB) =
    let die = fun () -> Error "expected right brace" in
    if USet.mem cLB digits then
      let rec firstBound xs (_i,cFB) =
        if USet.mem cFB digits then ParseWith (firstBound (xs*10+(UChar.int_of cFB - Char.code '0')),die) else
        if cFB = u ',' then ParseWith (startSecondBound xs,die) else
        if cFB = u '}' then post pieces (u '{') (PBound (xs,Some xs)) else
        unexpected cFB
      and startSecondBound xs (_i,cSSB) =
        if USet.mem cSSB digits then ParseWith (secondBound xs (UChar.int_of cSSB-Char.code '0'),die) else
        if cSSB = u '}' then post pieces (u '{') (PBound (xs,None)) else
        unexpected cSSB
      and secondBound xs ys (_i,cSB) =
        if USet.mem cSB digits then ParseWith (secondBound xs (ys*10+(UChar.int_of cSB - Char.code '0')),die) else
        if cSB = u '}' then
          if xs <= ys then post pieces (u '{') (PBound (xs,Some ys))
          else ParseError "repetition upper bound is less than lower bound"
        else
        unexpected cSB
      in ParseWith (firstBound (UChar.int_of cLB-Char.code '0'),die)
    else
    nextPiece ((PAtom (PChar (u '{')),iLB-1)::pieces) (iLB,cLB)
    (* A leading ^ inverts the sense of the bracket *)
    (* [: class :] [. collating element .] [= equivalvence class of collating elements =] *)
(*
     To include a literal `]' in the list, make it the first character (following a possible `^').  To include a literal `-',
     make it the first or last character, or the second endpoint of a range.  To use a literal `-' as the first endpoint of a
     range, enclose it in `[.' and `.]' to make it a collating element (see below).  With the exception of these and some com-
     binations using `[' (see next paragraphs), all other special characters, including `\', lose their special significance
     within a bracket expression.
*)
    (* Establish lexical context with "pieces" which is built in reverse order *)
  and leftBracket pieces (iLB,cLB) =
    let die = fun () -> Error "expected right bracket" in
    let diePB c = fun () -> Error (spr "expected closing %c and ]" (UChar.char_of c)) in
    (* Establish lexical context with "invert" *)
    let rec startBracket invert (iSB,cSB) =
      (* Establish lexical context with "xs" which is built in reverse order *)
      let rec inBracket xs (_i,cIB) =
        (* define how to hand [: :] and [. .] and [= =] *)
        let rec postBracket (iPB,cPB) = 
          if cPB = u ':' then ParseWith (parseUntil cPB tryClass,diePB cPB) else
          if cPB = u '.' then ParseWith (parseUntil cPB tryColl,diePB cPB) else
          if cPB = u '=' then ParseWith (parseUntil cPB tryEquiv,diePB cPB) else
          inBracket (BPChar (BChar cIB)::xs) (iPB,cPB)
        (* Establish lexical context with "flag" and "cont" *)
        and parseUntil flag cont (iPU,cPU) =
          (* cs is a list of UChar in reverse order *)
          let rec grab cs (_i,cG) = if cG <> flag then ParseWith (grab (cG::cs),diePB flag)
            else if cs = [] then ParseError (spr "Expecting non %c and non ]" (UChar.char_of flag))
                 else ParseWith (mustEnd cs,die)
          and mustEnd cs (_i,cME) = 
            if cME = u ']' then cont (ee (List.rev cs))
            else ParseError (spr "expected closing %c and ]" (UChar.char_of flag))
          in grab [] (iPU,cPU)
        (* Traditionally the classes are implemented by sets of ASCII characters *)
        (* http://www.regular-expressions.info/posixbrackets.html *)
        and tryClass str = if UTF8Set.mem str allowedClasses
          then ParseWith (inBracket (BPSet (BClassElem str)::xs),die)
          else ParseError "invalid character class name in [:bracket:]"
        (* This is parsed but only accepts a single unicode character *)
        and tryColl str =
          if UTF8.length str = 1 then ParseWith (inBracket (BPChar (BCollatingElem (UTF8.get str 0))::xs),die)
          else ParseError "invalid collating element, only single characters in [. .] accepted"
        (* This is parsed but only accepts a single unicode character *)
        and tryEquiv str =
          if UTF8.length str = 1 then ParseWith (inBracket (BPSet (BEquivClassElem (UTF8.get str 0))::xs),die)
          else ParseError "invalid collating equivalence element, only single characters in [= =] accepted"
        (* after a '-' there is a difficulty with '[' *)
        and dashRange (_i,cDR) =
          if cDR = u ']' then endBracket (BPChar (BChar (u '-'))::xs) else
          if cDR = u '[' then ParseWith (dashPostBracket,die) else
          makeRange (BChar cDR) (fun newXS -> ParseWith (inBracket newXS,die))
        (* handle the difficulty of '-' followed by '[' *)
        and dashPostBracket (iDPB,cDPB) =
          if cDPB = u ':' then ParseError "invalid end of range" else
          if cDPB = u '.' then ParseWith (parseUntil cDPB tryDashColl,diePB cDPB) else
          if cDPB = u '=' then ParseError "invalid end of range" else
          makeRange (BChar (u '[')) (fun newXS -> inBracket newXS (iDPB,cDPB))
        (* accept the difficult [x-[.y.]] syntax where all four of [ - [ . trigger nested parsers *)
        and tryDashColl str =
          if UTF8.length str = 1 then 
            let cDC = UTF8.get str 0 in
            makeRange (BCollatingElem cDC) (fun newXS -> ParseWith (inBracket newXS,die))
          else ParseError "invalid collating element, only single characters in [. .] accepted"
        (* sanity check the dashed range and used passed continuation *)
        (* If the first element is a collating [. .] bracket it is converted into a normal character *)
        and makeRange cRight cont =
          match xs with
              (BPChar (BChar c)::_) when c = u '-' -> ParseError "starting point of range cannot be '-'"
            | (BPChar cLeft::ys) -> if compareBC cLeft cRight <= 0 then cont (BPRange (cLeft,cRight)::ys)
                else ParseError "the end point of the dashed character range is less than the starting point"
            | (BPRange _::_) -> ParseError "a dash cannot follow a dashed range in a bracket"
            | (BPSet _::ys) -> ParseError "cannot use a character set as a starting point of range"
            | [] -> ParseError "a dash before here is not allowed" (* probably impossible to trigger *)
        in
        if cIB = u ']' then endBracket xs else
        if cIB = u '[' then ParseWith (postBracket,die)  else
        if cIB = u '-' then ParseWith (dashRange,fun () -> Error "expected end of range or right bracket") else
        ParseWith (inBracket (BPChar (BChar cIB)::xs),die)
      and endBracket xs =
        let data = (PAtom (PBracket (invert,List.rev xs)),iLB-1)::pieces in
        ParseWith (nextPiece data,endPiece data)
      in
      if cSB = u ']' then ParseWith (inBracket [BPChar (BChar cSB)],die) else
      if cSB = u '-' then ParseWith (inBracket [BPChar (BChar cSB)],die) else
      inBracket [] (iSB,cSB)
    in
    if cLB = u '^' then ParseWith (startBracket true,die) else
    startBracket false (iLB,cLB)
  and endPiece pieces () = match (List.rev pieces) with
      [] -> failwith "Impossible use of endPiece function with no pieces when parsing a regular expression"
    | (p::ps) -> endBranch ((p,ps)::branches)
  and endBranch endBranches = match env.popCont with
      None -> Ok (List.rev endBranches)
    | Some _ -> Error "expected right parenthesis"
  in (* kick off pushBranch by calling nextPiece *)
  nextPiece [] (iPB,cPB)

let initialParser () = 
  let freshEnv = { parentGroupIndex = 0
                 ; nextGroupIndexRef = ref 1
                 ; popCont = None }
  in ParseWith (pushBranch freshEnv [],fun () -> Ok [])

let parseRegex : ustring -> parseEnd = fun s ->
(*  pr "IN : %s\n" s;*)
  iterParse (initialParser ()) s

let ick s = let pe = parseRegex s
            in (Printf.printf " OUT: %s\n" (see pe); pe)

