#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let brr = Conf.with_pkg "brr"

let () =
  Pkg.describe "note" @@ fun c ->
  let brr = Conf.value c brr in
  Ok [ Pkg.mllib "src/note.mllib";
       Pkg.mllib "src/brr/note_brr.mllib" ~cond:brr ~dst_dir:"brr/";
       Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
       Pkg.test "test/test";
       Pkg.test ~run:false "test/clock";
       Pkg.doc ~built:false "doc/note_ui_sample.png" ~dst:"odoc-assets/";
     ]
