type uchar = CamomileLibrary.UChar.t
val uchar_of_sexp : Sexplib.Sexp.t -> CamomileLibrary.UChar.uchar
val sexp_of_uchar : CamomileLibrary.UChar.uchar -> Sexplib.Sexp.t

type ustring = CamomileLibrary.UTF8.t
val ustring_of_sexp : Sexplib.Sexp.t -> string
val sexp_of_ustring : string -> Sexplib.Sexp.t

type uset = CamomileLibrary.USet.t
val printer_uset : Format.formatter -> CamomileLibrary.USet.t -> unit

(* val e : CamomileLibrary.UChar.t -> CamomileLibrary.UTF8.t *)

type bracketChar = BChar of uchar | BCollatingElem of uchar
val bracketChar_of_sexp__ : Sexplib.Sexp.t -> bracketChar
val bracketChar_of_sexp : Sexplib.Sexp.t -> bracketChar
val sexp_of_bracketChar : bracketChar -> Sexplib.Sexp.t

val getBC : bracketChar -> uchar

val compareBC : bracketChar -> bracketChar -> int

type bracketSet = BClassElem of ustring | BEquivClassElem of uchar
val bracketSet_of_sexp__ : Sexplib.Sexp.t -> bracketSet
val bracketSet_of_sexp : Sexplib.Sexp.t -> bracketSet
val sexp_of_bracketSet : bracketSet -> Sexplib.Sexp.t

type bracketPiece =
    BPChar of bracketChar
  | BPRange of bracketChar * bracketChar
  | BPSet of bracketSet
val bracketPiece_of_sexp__ : Sexplib.Sexp.t -> bracketPiece
val bracketPiece_of_sexp : Sexplib.Sexp.t -> bracketPiece
val sexp_of_bracketPiece : bracketPiece -> Sexplib.Sexp.t

type atomPat =
    PDot
  | PEscape of uchar
  | PChar of uchar
  | PBracket of bool * bracketPiece list
  | PGroup of pGroup
and pGroup = {
  parentGI : Common.groupIndex;
  thisGI : Common.groupIndex;
  subPattern : pattern;
}
and anchorPat = PCarat | PDollar
and repPat = PQuest | PPlus | PStar | PBound of int * int option
and elemPat =
    PAtom of atomPat
  | PAnchor of anchorPat
  | PRepeat of atomPat * repPat
and elemAt = elemPat * Common.patIndex
and branchPat = elemAt * elemAt list
and pattern = branchPat list

val show_pattern : pattern -> CamomileLibrary.UTF8.t

val atomPat_of_sexp__ : Sexplib.Sexp.t -> atomPat
val atomPat_of_sexp : Sexplib.Sexp.t -> atomPat
val pGroup_of_sexp__ : Sexplib.Sexp.t -> pGroup
val pGroup_of_sexp : Sexplib.Sexp.t -> pGroup
val anchorPat_of_sexp__ : Sexplib.Sexp.t -> anchorPat
val anchorPat_of_sexp : Sexplib.Sexp.t -> anchorPat
val repPat_of_sexp__ : Sexplib.Sexp.t -> repPat
val repPat_of_sexp : Sexplib.Sexp.t -> repPat
val elemPat_of_sexp__ : Sexplib.Sexp.t -> elemPat
val elemPat_of_sexp : Sexplib.Sexp.t -> elemPat
val elemAt_of_sexp__ : Sexplib.Sexp.t -> elemAt
val elemAt_of_sexp : Sexplib.Sexp.t -> elemAt
val branchPat_of_sexp__ : Sexplib.Sexp.t -> branchPat
val branchPat_of_sexp : Sexplib.Sexp.t -> branchPat
val pattern_of_sexp__ : Sexplib.Sexp.t -> pattern
val pattern_of_sexp : Sexplib.Sexp.t -> pattern
val sexp_of_atomPat : atomPat -> Sexplib.Sexp.t
val sexp_of_pGroup : pGroup -> Sexplib.Sexp.t
val sexp_of_anchorPat : anchorPat -> Sexplib.Sexp.t
val sexp_of_repPat : repPat -> Sexplib.Sexp.t
val sexp_of_elemPat : elemPat -> Sexplib.Sexp.t
val sexp_of_elemAt : elemAt -> Sexplib.Sexp.t
val sexp_of_branchPat : branchPat -> Sexplib.Sexp.t
val sexp_of_pattern : pattern -> Sexplib.Sexp.t
