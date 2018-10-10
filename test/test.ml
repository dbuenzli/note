(*---------------------------------------------------------------------------
   Copyright (c) 2018 The note programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Tests for note's combinators.
   Compile with -g to get a precise backtrace to the error.

   Note that the testing mechanism itself (cf. occs and vals) needs a correct
   implementation; particulary w.r.t. updates with side effects. *)

open Note

let strf = Printf.sprintf
let log f = Format.printf (f ^^ "@.")
let tr id v = Printf.printf "%s: %d\n%!" id v; v

let log_fail pp_val v v' rest =
  log "@[@[%a@] <> @[%a@] (assert) remaining: @[%a@]@]"
    pp_val v pp_val v'
    Format.(pp_print_list ~pp_sep:pp_print_space pp_val) rest

let log_fail_empty pp_val v = log "%a but no change expected" pp_val v

(* Tests the event e goes trough occs *)
let occs pp_val ?(eq = ( = )) e occs =
  let occs = ref occs in
  let assert_occ o = match !occs with
  | o' :: occs' when eq o o' -> occs := occs'
  | o' :: occs'  -> log_fail pp_val o o' occs'; assert false
  | [] -> log_fail_empty pp_val o; assert false
  in
  E.log e assert_occ, occs

let int_occs = occs Format.pp_print_int
let bool_occs = occs Format.pp_print_bool
let unit_occs = occs (fun ppf () -> Format.fprintf ppf "()")

(* Tests the signal s goes through vals *)
let vals pp_val ?(eq = ( = )) s vals =
  let vals = ref vals in
  let assert_val v = match !vals with
  | v' :: vals' when eq v v' -> vals := vals'
  | v' :: vals' -> log_fail pp_val v v' vals'; assert false
  | [] -> log "%a, but empty asserts" pp_val v; assert false
  in
  Some (S.log s assert_val), vals

let int_vals = vals Format.pp_print_int
let bool_vals = vals Format.pp_print_bool

(* To initialize asserts of dynamic creations. *)
let assert_e_stub () = ref (None, ref [])
let assert_s_stub v = ref (None, ref [])

(* Tests that we went through all vals or occs *)
let empty (_, r) = assert (!r = [])

let test_basic_event () =
  log "Basic event";
  let x, send_x = E.create () in
  let occs = [1;2;2;3;3;3] in
  let assert_x = int_occs x occs in
  List.iter send_x occs;
  List.iter empty [assert_x];
  ()

let test_e_map () =
  log "E.map";
  let x, send_x = E.create () in
  let occs = [1;2;2;3;3;3] in
  let twice = ( * ) 2 in
  let y = E.map twice x in
  let assert_twice = int_occs y (List.map twice occs) in
  List.iter send_x occs;
  List.iter empty [assert_twice];
  ()

let test_e_swap () =
  log "E.swap";
  let x, send_x = E.create () in
  let y, send_y = E.create () in
  let s, set_x = S.create 0 in
  let es = s |> S.map ~eq:( == ) @@ function
  | 0 -> E.map (fun x -> x) x
  | 1 -> y
  | _ -> assert false
  in
  let e = E.swap es in
  let assert_e = int_occs e [2;3;3;4;5;6] in
  send_x 2; send_y 5;
  set_x 1; send_x 2; send_y 3; send_y 3;
  set_x 0; send_y 5; send_x 4; send_x 5; send_x 6;
  List.iter empty [assert_e];
  ()

let test_basic_signal () =
  log "Basic signal";
  let x, set_x = S.create 1 in
  let assert_x = int_vals x [1;2;3;2;3] in
  List.iter set_x [2;2;3;2;3;3];
  List.iter empty [assert_x];
  ()

let test_s_hold () =
  log "S.hold";
  let x, send_x = E.create () in
  let s = S.hold 1 x in
  let assert_x = int_occs x [2;2;3;2;3;3]; in
  let assert_s = int_vals s [1;2;3;2;3] in
  List.iter send_x [2;2;3;2;3;3];
  List.iter empty [assert_x; assert_s];
  ()

let test_s_map () =
  log "S.map";
  let x, set_x = S.create 1 in
  let twice = S.map (fun x -> 2 * x) x in
  let assert_twice = int_vals twice [2;4;6;4;6] in
  List.iter set_x [2;2;2;3;2;3;3];
  List.iter empty [assert_twice];
  ()

let test_s_bind () =
  log "S.bind";
  let flip, set_flip = S.create true in
  let s0, set_s0 = S.create 0 in
  let s1, set_s1 = S.create 1 in
  let switch = function true -> s0 | false -> s1 in
  let b = S.bind flip switch  in
  let assert_b = int_vals b [0;1;3;2;3] in
  set_flip true; set_flip false;
  set_s0 2; set_s1 3;
  set_flip true;
  set_s1 2;
  set_flip false;
  set_s1 3;
  List.iter empty [assert_b];
  ()

let high_s s =
  let id s = S.map (fun v -> v) s in (id (id (id (id (id (id (id (id s))))))))

let test_s_changes () =
  log "S.changes";
  let e, send_e = E.create () in
  let s = S.hold 1 e in
  let c = S.changes s in
  let assert_dc = assert_e_stub () in
  let assert_dhc = assert_e_stub () in
  let dyn () =
    let dc = S.changes s in
    let dhc = S.changes (high_s s) in
    assert_dc := int_occs dc [4];
    assert_dhc := int_occs dhc [4]
  in
  let create_dyn = S.map (fun v -> if v = 3 then dyn ()) s in
  let log_dyn = S.log create_dyn (fun _ -> ()) in
  let assert_c = int_occs c [3; 4] in
  List.iter send_e [1; 1; 3; 3; 4; 4];
  List.iter empty [assert_c; !assert_dc; !assert_dhc];
  Logr.destroy log_dyn;
  ()

let test_s_init_dyn () =
  (* Tests init when created in a step. *)
  log "S dynamic creation initialization";
  let s0, set_s0 = S.create 0 in
  let s1, set_s1 = S.create 1 in
  let m0 = S.map (fun x -> x + 1) s0 in
  let m1 = S.map (fun x -> x + 1) s1 in
  let dyn0 = function _ -> S.map (fun x -> x + 1) m1 (* !! *)  in
  let dyn1 = function _ -> S.map (fun x -> x + 1) m0 (* !! *) in
  let d0 = S.bind s0 dyn0 in
  let d1 = S.bind s1 dyn1 in
  let assert_d0 = int_vals d0 [3;8] in
  let assert_d1 = int_vals d1 [2;7] in
  let step = Step.create () in
  set_s0 ~step 5;
  set_s1 ~step 6;
  Step.execute step;
  List.iter empty [assert_d0; assert_d1];
  ()

let test_s_bind_dyn () = (* dyn bind from react test suite *)
  log "S.bind dynamic";
  let s1, set_s1 = S.create true in
  let s2, set_s2 = S.create 1 in
  let bind1 = function
  | true ->
      let bind2 = function
      | true -> s2
      | false -> S.const 2
      in
      S.bind s1 bind2
  | false -> S.const 2
  in
  let s = S.bind s1 bind1 in
  let assert_bind = int_vals s [1; 2; 1] in
  set_s1 true;
  set_s1 false;
  set_s1 true;
  List.iter empty [assert_bind];
  ()

let test_s_bool () =
  log "S.Bool";
  let s, set_s = S.create 0 in
  let a_zedge = bool_occs (S.Bool.(edge false')) [] in
  let a_zrise = unit_occs (S.Bool.(rise false')) [] in
  let a_zfall = unit_occs (S.Bool.(fall false')) [] in
  let a_flip_never = bool_vals (S.Bool.flip false E.never) [false] in
  let flip = S.Bool.flip true (S.changes s) in
  let a_flip = bool_vals flip [true; false; true] in
  let a_flip_edge = bool_occs (S.Bool.edge flip) [false; true] in
  let a_flip_rise = unit_occs (S.Bool.rise flip) [()] in
  let a_flip_fall = unit_occs (S.Bool.fall flip) [()] in
  let dyn_flip = S.bind s (fun _ -> S.Bool.flip true (S.changes s)) in
  let a_dyn_flip = bool_vals dyn_flip [true] in
  let changes = S.changes s in
  let dyn_flip' = S.bind s (fun _ -> S.Bool.flip true changes) in
  let a_dyn_flip' = bool_vals dyn_flip' [true; false] in
  List.iter set_s [1;2;2];
  List.iter empty [a_flip_never; a_flip; a_dyn_flip; a_dyn_flip'; a_zedge;
                   a_flip_edge;  ];
  List.iter empty [a_zrise; a_zfall; a_flip_rise; a_flip_fall ];
  ()

(*
let test_s_delay () =
  log "Test delay";
  let int_list_vals =
    vals Format.(pp_print_list
            ~pp_sep:pp_print_space Format.pp_print_int)
  in
  let history s =
    let push v = function
    | [] -> [v]
    | v' :: _ as l when S.eq s v v' -> l
    | l -> v :: l
    in
    let rec h = lazy (S.l2 push s (S.delay [] h)) in
    Lazy.force h
  in
  let s, set_s = S.create 0 in
  let h = history s in
  let assert_h = int_list_vals h [[]; [1]; [1;2]] in
  List.iter set_s [1;1;2;2];
  List.iter empty [assert_h];
  ()
*)

let test_s_fix () =
  log "S.fix";
  let pp_comma ppf () = Format.fprintf ppf ",@ " in
  let pp_int_list = Format.(pp_print_list ~pp_sep:pp_comma pp_print_int)in
  let int_list_vals = vals pp_int_list in
  let history s =
    let push v = function
    | [] -> [v]
    | v' :: _ as l when S.eq s v v' -> l
    | l -> v :: l
    in
    S.fix [] (fun h -> let h' = S.l2 push s h in h', (h, h'))
  in
  let s, set_s = S.create 0 in
  let h_dt, h = history s in
  let incd = S.map (fun l -> List.map (( + ) 1) l) h_dt in
  let assert_h = int_list_vals h [[0]; [1;0]; [2;1;0]; [3;2;1;0]] in
  let assert_incd = int_list_vals incd [[1]; [2;1]; [3;2;1]; [4;3;2;1]] in
  List.iter set_s [1;1;2;2;3];
  List.iter empty [assert_h; assert_incd];
  ()

let test_s_fix_2 () =
  log "S.fix";
  let pairs s =
    let def v =
      let t = S.map (fun v -> v * 2) v in
      let v = S.map (fun v -> v) v in
      let v' = S.l2 (fun _ v -> if v mod 2 = 0 then v else v + 1) v s in
      v', (t, v')
    in
    S.fix 0 def
  in
  let s, set_s = S.create 0 in
  let t, p = pairs s in
  let assert_t = int_vals t [0; 4; 8] in
  let assert_p = int_vals p [0; 2; 4] in
  List.iter set_s [1;1;2;2;3];
  List.iter empty [assert_t; assert_p;];
  ()

let test_observation () =
  log "Test observation";
  let test ~now =
    let obs = [(Some 1, 1); (None, 2)] in
    let obs = if now then (None, 0) :: obs else obs in
    let obs = ref obs in
    let assert_obs e s =
      if (e, s) <> List.hd !obs then assert false;
      obs := List.tl !obs
    in
    let s, set_s = S.create 0 in
    let e = E.filter (( = ) 1) (S.changes s) in
    let log = Logr.(create ~now (const assert_obs $ E.obs e $ S.obs s)) in
    List.iter set_s [1;2;2];
    assert (!obs = []);
    Sys.opaque_identity @@ ignore (log) (* Avoid gc. *)
  in
  test ~now:true;
  test ~now:false;
  ()

let test_signals () =
  test_basic_event ();
  test_e_map ();
  test_e_swap ();
  test_basic_signal ();
  test_s_hold ();
  test_s_map ();
  test_s_bind ();
  test_s_changes ();
  test_s_init_dyn ();
  test_s_bind_dyn ();
  test_s_bool ();
  test_s_fix ();
  test_s_fix_2 ();
  test_observation ();
  ()

let main () =
  test_signals ();
  print_endline "All tests succeeded."

let () = main ()

(*---------------------------------------------------------------------------
   Copyright (c) 2018 The note programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
