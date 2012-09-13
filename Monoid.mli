module type TYPE = sig type t end

module type MONOIDF =
  functor (Type : TYPE) ->
    sig
      type t = Type.t
      val mempty : t
      val mappend : t -> t -> t
      val mconcat : t list -> t
    end

module Endo :
  functor (Type : TYPE) ->
    sig
      type t = Type.t -> Type.t
      val mempty : 'a -> 'a
      val mappend : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
      val mconcat : ('a -> 'a) list -> 'a -> 'a
    end
