(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brr programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Brr
open Note
open Note_brr
open Note_brr_kit

let key_state id is_down =
  let kbd = El.kbd [El.txt (Jstr.v id)] in
  Elr.def_class (Jstr.v "down") is_down kbd;
  kbd

let key_dir down up =
  let show_dir = El.span [] in
  let show d _ = [El.txt' (match d with `Up -> "↑" | `Down -> "↓")] in
  let dir = E.select [ E.map (show `Down) down; E.map (show `Up) up ] in
  Elr.set_children show_dir ~on:dir;
  show_dir

let key_id ev =
  let kbd = El.kbd [El.txt' "  "] in
  let id e = [El.txt (Key.to_jstr e)] in
  Elr.set_children kbd ~on:(E.map id ev);
  kbd

let key_viz evs = El.div [
    El.p [ key_id (E.select [Key.any_down evs; Key.any_up evs ]);
           key_dir (Key.any_down evs) (Key.any_up evs) ];
    El.p [ key_state "Any" (Key.any_holds evs) ];
    El.p [ key_state "Shift" (Key.shift evs);
           key_state "L" (Key.holds evs (`Shift `Left));
           key_state "R" (Key.holds evs (`Shift `Right)) ];
    El.p [ key_state "Ctrl" (Key.ctrl evs);
           key_state "L" (Key.holds evs (`Ctrl `Left));
           key_state "R" (Key.holds evs (`Ctrl `Right)) ];
    El.p [ key_state "Alt" (Key.alt evs);
           key_state "L" (Key.holds evs (`Alt `Left));
           key_state "R" (Key.holds evs (`Alt `Right)) ];
    El.p [ key_state "Meta" (Key.meta evs);
           key_state "L" (Key.holds evs (`Meta `Left));
           key_state "R" (Key.holds evs (`Meta `Right)) ];
    El.p [ key_state "Spacebar" (Key.holds evs `Spacebar);
           key_dir (Key.down evs `Spacebar) (Key.up evs `Spacebar)];
  ]

let main () =
  let h1 = El.h1 [El.txt' "Keyboard test"] in
  let info = El.p [El.txt' "Hit your keyboard."] in
  let body = Document.body G.document in
  El.set_children body [h1; info; key_viz (Key.on_el body)]

let () = main ()
