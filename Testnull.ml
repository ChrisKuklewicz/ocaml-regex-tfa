(* Testab.ml *)

(*open Common*)
(*open ReadPattern*)
(*open CorePattern*)
open Simulate
(*open Core.Result*)
(*open SimStep*)


let test () =
  let re = "a[ab]{20}a" in
  let text = stringToList (input_line stdin) in
  Printf.printf "%d %d\n" (String.length re) (List.length text);;

test ()
