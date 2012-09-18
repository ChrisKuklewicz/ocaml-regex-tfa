(* UnitTestFlush.ml *)

(*open Sexplib.Std*)
(*open Common*)
(*open Pattern*)
open ReadPattern
open CorePattern
(*open Simulate*)
open SimFlush
open Buffer
open Core.Result

TYPE_CONV_PATH "UnitTestFlush"

let manifest="test-manifest.txt"

let format_match (g,_) =
  let b = Buffer.create 12 in
  let add_match (l,r) =
    begin
      add_char b '(';
      (match l with
          (-1) -> add_char b '?'
        | x -> add_string b (string_of_int x));
      add_char b ',';
      (match r with
          (-1) -> add_char b '?'
        | x -> add_string b (string_of_int x));
      add_char b ')';
    end
  in
  Array.iter add_match g;
  contents b

let runTestFile name =
  let storeSAME = ref "" in
  let runTest n sREIn sTextIn sOut =
    let sN = string_of_int n in
    let sRE = if sREIn = "SAME" then !storeSAME
      else (storeSAME := sREIn ; sREIn)
    in
    match (parseRegex sRE) with
        Error err -> Printf.printf "Failed to parse: %s %s\nError message: %s\n" sN sRE err
      | Ok p ->
        (* let s = Sexplib.Sexp.to_string_hum (sexp_of_pattern p) in Printf.printf "Pattern %s\n" s;*)
        let cr = toCorePattern p in
        let sText = if sTextIn="NULL" then "" else sTextIn in
        let found = match uWrapFlush cr sText with
            [] -> "NOMATCH"
          | (h::_) -> format_match h
        in
        if (found=sOut)
        then
          if (0<=n)
          then Printf.printf "expected pass %s\n %s %s %s\n" sN sRE sText sOut
          else Printf.printf "UNEXPECTED PASS %s\n %s %s %s\n" sN sRE sText sOut
        else
          if (n<0)
          then Printf.printf "expected failed %s\n %s %s %s : %s\n" sN sRE sText sOut found
          else Printf.printf "UNEXPECTED FAILED %s\n %s %s %s : %s\n" sN sRE sText sOut found
  in
  let readTest (lineIn : string) = Scanf.sscanf lineIn " %d %s %s %s" runTest
  in
  let g = open_in name
  in
  Printf.printf "\nLoading Tests from: %s\n" name;
  Core.In_channel.iter_lines g ~f:readTest;
  close_in g

let runAllTestFiles () = 
  let chan = open_in manifest in
  Core.In_channel.iter_lines ~fix_win_eol:true chan ~f:runTestFile;
  ();;


Printf.printf "unitTestFlush\n";;
runAllTestFiles ();;
