(*---------------------------------------------------------------------------
   Copyright (c) 2018 The note programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Brr
open Note
open Note_brr

(* This tests that nodes removed from the HTML DOM destroy their log.  If
   they didn't they would never be garbage collected. *)

let count, run_count =
  let v = ref 0 in
  let count, send_count = E.create () in
  let rec run_count () =
    incr v; send_count !v; ignore (G.set_timeout ~ms:0 run_count)
  in
  count, run_count

let count_value count =
  (* Voluntarily silly. *)
  let p = El.p [] in
  let count_txt c = [El.txt Jstr.(v "Steps: " + of_int c)] in
  let count = S.hold [] (E.map count_txt count) in
  Elr.def_children p count;
  p

let count_value_nest count =
  let p = El.p [] in
  let count_txt c = [El.txt Jstr.(v "Steps (nest): " + of_int c)] in
  let count = S.hold [] (E.map count_txt count) in
  Elr.def_children p count;
  El.div [p]

let steps () =
  let steps = El.div [] in
  let children =
    let counts c = [count_value count; count_value_nest count] in
    S.hold [] (E.map counts count)
  in
  Elr.def_children steps children;
  steps

let main () =
  let h1 = El.h1 [El.txt' "No leaks!"] in
  let i = "Memory usage must be bounded and the counters must not slow down." in
  let info = El.p [El.txt' i ] in
  El.set_children (Document.body G.document) [h1; info; steps ()];
  run_count ()

let () = main ()
