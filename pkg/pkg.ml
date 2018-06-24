#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "note" @@ fun c ->
  Ok [ Pkg.mllib "src/note.mllib";
       Pkg.test "test/test";
       Pkg.test ~run:false "test/clock"; ]
