(* Testab.ml *)

open Common
open ReadPattern
open CorePattern
open Simulate
open Core.Result
open SimStep

let test () =
  let re = "a[ab]{20}a" in
  let text = input_line stdin in
  match wrapSimStep re text with
      [] -> Printf.printf "NOMATCH\n"
    | ((g,_)::_) -> 
      let s = (Sexplib.Sexp.to_string_hum (sexp_of_groupCap g) : string)
      in Printf.printf "%s\n" s;;

test ()
