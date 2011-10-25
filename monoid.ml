(* Monoid.ml *)

(* This is currently used in whichtest.ml *)

module type TYPE =
sig
  type t
end

module type MONOIDF =
  functor (Type : TYPE) ->
sig
  type t = Type.t
  val mempty : t
  val mappend : t -> t -> t
  val mconcat : t list -> t
end

module Endo = functor (Type : TYPE) ->
struct
  type t = Type.t -> Type.t
  let mempty = function x -> x
  let mappend = fun f g -> function x -> f (g x)
  let mconcat = function xs -> List.fold_left mappend mempty xs
end

