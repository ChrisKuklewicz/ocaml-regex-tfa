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

open Common
open CorePattern

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

type runPattern =
    ROr of runQ list
  | RSeq of runQ*runQ
  | RRepeat of runQ
  | RTest
  | ROneChar of (repMap ref)
  | RCaptureGroup of runQ

and runQ = { mutable active : bool
           ; mutable final : repMap
           ; contTo : runQ continueTo
           ; mutable getR : runPattern
           ; getQ : coreQ
           }

let rNothing = { active = false
               ; final = RepMap.empty
               ; contTo = ContRoot
               ; getR = RTest
               ; getQ = CorePattern.nothing }

let rec convertCore = fun (c : runQ continueTo) q ->
  let self = { rNothing with getQ = q; contTo = c } in
  let _ = match q.unQ with
      Or qs -> self.getR <- ROr (List.map (convertCore(ContReturn self)) qs)
    | Seq (qFront,qEnd) ->
      let rEnd = convertCore c qEnd 
      in self.getR <- RSeq (convertCore (ContEnter rEnd) qFront,rEnd)
    | Repeat r -> self.getR <- RRepeat (convertCore (ContReturn self) r.unRep)
    | Test _ -> self.getR <- RTest
    | OneChar _ -> let r = ref (RepMap.empty) 
                   in self.getR <- ROneChar r
    | CaptureGroup cg -> self.getR <- RCaptureGroup (convertCore (ContReturn self) cg.subPat)
  in self

                                                                                                                                                
