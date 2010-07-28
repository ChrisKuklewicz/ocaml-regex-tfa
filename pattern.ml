(* pattern.ml defines the pattern type for storing parsed regular expression *)
open CamomileLibrary
open UPervasives
open Common

TYPE_CONV_PATH "Pattern"

type uchar = CamomileLibrary.UChar.t
type ustring = UTF8.t
type uset = USet.t

let uchar_of_sexp s = UChar.of_int (Sexplib.Conv.int_of_sexp s)
let sexp_of_uchar u = Sexplib.Conv.sexp_of_int (UChar.int_of u)
let ustring_of_sexp s = Sexplib.Conv.string_of_sexp s
let sexp_of_ustring u = Sexplib.Conv.sexp_of_string u

(* Convert a single UChar to a UTF8 encoded string *)
let e c = let b = UTF8.Buf.create 4 in
          UTF8.Buf.add_char b c;
          UTF8.Buf.contents b;;

(* USet.fold_range has a typo, corrent here *)
let fold_range f s a =
  let f' n1 n2 a = f (UChar.chr_of_uint n1) (UChar.chr_of_uint n2) a in
  ISet.fold_range f' s a

(* ISet.remove has a typo, correct here *)
let rec myRemove n s =
  if AvlTree.is_empty s then AvlTree.empty else
  let (v1, v2) as v = AvlTree.root s in
  let s1 = AvlTree.left_branch s in
  let s2 = AvlTree.right_branch s in
  if n < v1 then AvlTree.make_tree (myRemove n s1) v s2
  else if n = v1 then
    if v1 = v2 then AvlTree.concat s1 s2 else
    AvlTree.make_tree s1 (v1 + 1, v2) s2
  else if n > v1 && n < v2 then
    let s = AvlTree.make_tree s1 (v1, n - 1) AvlTree.empty in
    AvlTree.make_tree s (n + 1, v2) s2
  else if n = v2 then AvlTree.make_tree s1 (v1, v2 - 1) s2 else
  AvlTree.make_tree s1 v (myRemove n s2)

(* Wrap myRemove to replace USet.remove *)
let uremove u s = USet.uset_of_iset (myRemove (UChar.uint_code u) (USet.iset_of_uset s))

let printer_uset = fun f s ->
  Format.fprintf f "@[<1>(uset(%d) " (USet.cardinal s);
(* XXX XXX There is a bug in USet.fold_range, r is always the same as l XXX XXX *)
  ignore (fold_range (fun l r first ->
    if not first then Format.fprintf f "," else ();
    if l==r then Format.fprintf f "%a" printer_uchar l
    else Format.fprintf f "%a-%a" printer_uchar l printer_uchar r;
    false;) (USet.iset_of_uset s) true);
  Format.fprintf f ")@]";;

type bracketChar =
    BChar of uchar
  | BCollatingElem of uchar
with sexp
type bracketSet =
    BClassElem of ustring
  | BEquivClassElem of uchar
with sexp
type bracketPiece =
    BPChar of bracketChar
  | BPRange of bracketChar*bracketChar
  | BPSet of bracketSet
with sexp

let getBC = function BChar a | BCollatingElem a -> a
let compareBC a b = compare (getBC a) (getBC b)

(*
  The pattern type faithfully records valid string regular expressions.
  EXCEPT that leading zeros in {n,m} repeats are not recorded.
  EXCEPT that {i,i} is recorded the same as {i}.

  The show_pattern function faithfully reproduces the input string regular expression.
  EXCEPT that leading zeros in {n,m} repeats are not reproduced.
  EXCEPT that {i,i} is reproduced as {i}.

  This pattern type is a much more restrictive design that in the Haskell Pattern.hs code.  The type
  below enforces policies that the Haskell type does not enforce: that branches must be non-empty
  and that use of pattern/alternation/concatenation must be in the correct order and go though
  PGroup capture to recurse, and it is impossible to repeat an anchor or another repetition.
*)


type atomPat =
    PDot
  | PEscape of uchar  (* Some escape sequences may be interpreted at a higher level *)
  | PChar of uchar
  | PBracket of bool*(bracketPiece list) (* bool is true if inverted *)
  | PGroup of pGroup
and pGroup = { parentGI : groupIndex; thisGI : groupIndex; subPattern : pattern }
and anchorPat = PCarat | PDollar
and repPat = PQuest | PPlus | PStar | PBound of int * (int option)
and elemPat =
    PAtom of atomPat
  | PAnchor of anchorPat
  | PRepeat of atomPat*repPat         (* Cannot repeat anchors, cannot repeat repeats *)
and elemAt = elemPat*patIndex         (* For debugging, records pattern index for elements *)
and branchPat = elemAt*(elemAt list)  (* Enforce non-empty branches *)
and pattern = branchPat list          (* regular expression patterns may be empty *)
with sexp


let show_pattern patternIn =
  let buf = UTF8.Buf.create 15 in
  let add_uchar c = UTF8.Buf.add_char buf c
  and add_int i = UTF8.Buf.add_string buf (Printf.sprintf "%d" i) in
  let add_char c = add_uchar (UChar.of_char c) in
  let rec render = function
    | [] -> ()
    | (x::xs) -> doBranch x ; List.iter (fun q -> add_char '|' ; doBranch q) xs
  and doBranch (b,bs) = List.iter doElemAt (b::bs)
  and doElemAt (e,_) = doElem e
  and doElem = function
    | PAtom a -> doAtom a
    | PAnchor PCarat -> add_char '^'
    | PAnchor PDollar -> add_char '$'
    | PRepeat (a,r) -> doAtom a ; doRep r
  and doAtom = function
    | PDot -> add_char '.'
    | PEscape c ->  add_char '\\' ; add_uchar c
    | PChar c -> add_uchar c
    | PBracket (i,b) -> doBracket (i,b)
    | PGroup {subPattern=p} -> add_char '(' ; render p ; add_char ')'
  and doRep = function
    | PQuest -> add_char '?'
    | PPlus -> add_char '+'
    | PStar -> add_char '*'
    | PBound (i,None) -> add_char '{'; add_int i; add_char ','; add_char '}'
    | PBound (i,Some j) when i==j -> add_char '{' ; add_int i; add_char '}'
    | PBound (i,Some j) -> add_char '{' ; add_int i;  add_char ',';  add_int j; add_char '}'
  and doBracket (invert,bps) =
    begin
      add_char '[';
      if invert then add_char '^' else ();
      List.iter doBP bps;
      add_char ']';
    end
  and doBP = function
    | BPChar bc -> doBC bc
    | BPRange (bc1,bc2) -> doBC bc1 ; add_char '-'; doBC bc2
    | BPSet bs ->
      begin
        add_char '[';
        (match bs with
            BClassElem s -> add_char ':' ; UTF8.Buf.add_string buf s ; add_char ':';
          | BEquivClassElem c -> add_char '=' ; add_uchar c ; add_char '=';);
        add_char ']';
      end
  and doBC = function
    | BChar c -> add_uchar c
    | BCollatingElem c -> add_char '['; add_char '.'; add_uchar c; add_char '.'; add_char ']'
  in render patternIn ; UTF8.Buf.contents buf
