(*---------------------------------------------------------------------------
   Copyright (c) 2018 The note programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

module rec Src : sig (* Sources where data is pushed *)
  type t = V : _ typed -> t
  and 'a typed
  val compare : t -> t -> int
  val id : t -> int
  val cell : 'a typed -> 'a C.t
  val untyped : 'a typed -> t
  val logrs : t -> Logr.t list
  val add_logr : Logr.t -> t -> unit
  val rem_logr : Logr.t -> t -> unit
  val reset_stamp : t -> unit
  val find_active_step : Step.t -> Srcs.t -> Step.t
  val create : ?eq:('a -> 'a -> bool) -> 'a -> 'a typed
end = struct
  type t = V : _ typed -> t
  and 'a typed =
    { id : int; (* unique id for the source *)
      cell : 'a C.t; (* Cell holding the source's value *)
      mutable logrs : Logr.t list (* loggers that depend on the source *);
      self : t; (* self, untyped. *) }

  let id (V s) = s.id
  let cell s = s.cell
  let untyped s = s.self
  let compare (V s) (V t) = (Stdlib.compare : int -> int -> int) s.id t.id
  let logrs (V s) = s.logrs
  let add_logr logr (V s) = s.logrs <- logr :: s.logrs
  let rem_logr logr  (V s) =
    let rec rem logr acc = function
    | [] -> acc
    | v :: vs when v == logr -> List.rev_append vs acc
    | v :: vs -> rem logr (v :: acc) vs
    in
    s.logrs <- rem logr [] s.logrs

  let reset_stamp (V s) = C.set_stamp s.cell Step.nil

  exception Step of Step.t
  let find_active_step step ss =
    if step != Step.nil then step else
    let find_not_nil (V s) =
      let step = C.stamp s.cell in
      if step != Step.nil then raise_notrace (Step step)
    in
    try Srcs.iter find_not_nil ss; Step.nil with Step s -> s

  let uid = let id = ref 0 in fun () -> incr id; !id
  let create ?eq v =
    let update _ _ = () in
    let cell = C.create ?eq ~step:Step.nil ~srcs:Srcs.empty v ~update in
    let rec src = { id = uid (); cell; logrs = []; self = V src } in
    C.set_srcs cell (Srcs.singleton src.self);
    C.set_srcs_changed cell false;
    src
end

and Srcs : Set.S with type elt = Src.t = Set.Make (Src)
and C : sig (* Cells *)
  type 'a t
  type untyped = C : 'a t -> untyped
  val create :
    ?eq:('a -> 'a -> bool) -> step:Step.t -> srcs:Srcs.t -> 'a ->
    update:(Step.t -> 'a t -> unit) -> 'a t

  val const : ?eq:('a -> 'a -> bool) -> 'a -> 'a t
  val eq : 'a t -> ('a -> 'a -> bool)
  val set_eq : 'a t -> ('a -> 'a -> bool) -> unit
  val with_eq : ('a -> 'a -> bool) -> 'a t -> 'a t
  val stamp : 'a t -> Step.t
  val set_stamp : 'a t -> Step.t -> unit
  val srcs : 'a t -> Srcs.t
  val srcs_changed : 'a t -> bool
  val set_srcs : 'a t -> Srcs.t -> unit
  val set_srcs_changed : 'a t -> bool -> unit
  val value : 'a t -> 'a
  val value_changed : 'a t -> bool
  val set_value : 'a t -> 'a -> unit
  val update : Step.t -> 'a t -> unit
  val set_update : 'a t -> (Step.t -> 'a t -> unit) -> unit
  val src_update : Step.t -> 'a t -> 'a -> bool
  val up_to_date_value : 'a t -> 'a

  val create_instant :
    step:Step.t -> srcs:Srcs.t -> 'a option ->
    update:(Step.t -> 'a option t -> unit) -> 'a option t

  val reset_instant : 'a option t -> unit
  val set_instant : Step.t -> 'a option t -> 'a option -> unit

  val delay :  'a -> 'a t Lazy.t -> 'a t
  val fix : ?eq:('a -> 'a -> bool) -> 'a -> ('a t -> 'a t * 'b) -> 'b
  val defer : 'a -> 'a t -> 'a t
  val dump_src_ids : Format.formatter -> 'a t -> unit
end = struct
  type 'a t =
    { mutable eq : 'a -> 'a -> bool; (* testing for cell value equality *)
      mutable stamp : Step.t; (* last step in which the cell updated *)
      mutable srcs : Srcs.t; (* sources the cell depends on *)
      mutable srcs_changed : bool; (* [true] if [srcs] changed *)
      mutable value : 'a; (* cell value *)
      mutable value_changed : bool; (* [true] if [value] changed *)
      mutable update : Step.t -> 'a t -> unit; }(* updates [value] and [srcs] *)

  type untyped = C : 'a t -> untyped

  let create ?(eq = ( = )) ~step ~srcs value ~update =
    { eq; stamp = step; srcs; srcs_changed = true; value; value_changed = true;
      update }

  let const ?(eq = ( = )) v =
    { eq; stamp = Step.nil; srcs = Srcs.empty; srcs_changed = false;
      value = v; value_changed = false; update = (fun _ _ -> ()) }

  let eq c = c.eq
  let set_eq c eq = c.eq <- eq
  let with_eq eq c = { c with eq }
  let stamp c = c.stamp
  let set_stamp c stamp = c.stamp <- stamp
  let srcs_changed c = c.srcs_changed
  let set_srcs_changed c bool = c.srcs_changed <- bool
  let srcs c = c.srcs
  let set_srcs c srcs = c.srcs_changed <- true; c.srcs <- srcs
  let value c = c.value
  let value_changed c = c.value_changed
  let set_value c v =
    if c.eq v c.value then () else (c.value_changed <- true; c.value <- v)

  let update step c =
    if step != Step.nil && c.stamp != step then begin
      c.stamp <- step; c.srcs_changed <- false; c.value_changed <- false;
      (* XXX would be nice to avoid constructing the set *)
      if Srcs.(is_empty (inter c.srcs (Step.srcs step)))
      then () (* no need to go there, nothing can update *)
      else
      c.update step c
    end

  let set_update c u = c.update <- u
  let src_update step c v =
    c.value_changed <- false;
    if c.eq v c.value
    then false
    else (c.stamp <- step; c.value_changed <- true; c.value <- v; true)

  let up_to_date_value c =
    let step = Src.find_active_step Step.nil c.srcs in
    update step c; c.value

  let reset_instant c = c.value_changed <- false; c.value <- None
  let set_instant step c = function
  | None -> ()
  | Some _ as v ->
      c.value_changed <- true; c.value <- v;
      Step.add_cleanup step (fun () -> reset_instant c)

  let create_instant ~step ~srcs value ~update =
    let value_changed = match value with None -> false | Some _ -> true in
    let c =
      { eq = ( = ); stamp = step; srcs; srcs_changed = true; value;
        value_changed; update }
    in
    if value_changed && step <> Step.nil
    then Step.add_cleanup step (fun () -> reset_instant c);
    c

  let delay i z = failwith "TOOD"
  let fix ?eq i cf =
    let src = Src.create ?eq i in
    let src = Src.V src and d = Src.cell src in
    let c, r = cf d in
    let c_update = c.update in
    let c_update step self =
      c_update step self;
      if c.value_changed then (Step.add_delayed step src);
    in
    let d_update step self =
      if step == Step.delayed then set_value self (value c) else ()
    in
    c.update <- c_update;
    d.update <- d_update;
    let step = Src.find_active_step Step.nil (C.srcs c) in
    let () = update step c in
    if step == Step.nil then Step.execute_delayed (Srcs.singleton src);
    r

  let defer init c =
    (** XXX do we really need a source for that. *)
    let src = Src.create ~eq:c.eq init in
    let src = Src.V src and d = Src.cell src in
    let update step self =
      if step == Step.delayed
      then set_value self (value c)
      else begin
        C.(update step c);
        if C.srcs_changed c then C.set_srcs d (C.srcs c);
        if C.value_changed c then Step.add_delayed step src
      end
    in
    d.update <- update;
    let step = Src.find_active_step Step.nil (srcs c) in
    let () = update step c in
    let () = update step d in
    if step == Step.nil then Step.execute_delayed (Srcs.singleton src);
    d

  let dump_src_ids ppf c =
    Format.fprintf ppf "@[{%a}@]"
      Format.(pp_print_list ~pp_sep:pp_print_space pp_print_int)
      (List.map (fun s -> Src.id s) (Srcs.elements c.srcs))
end

and Logr : sig
  type 'a obs
  val const : 'a -> 'a obs
  val obs_cell : 'a C.t -> 'a obs
  val app : ('a -> 'b) obs -> 'a obs -> 'b obs
  val ( $ ) : ('a -> 'b) obs -> 'a obs -> 'b obs
  type t
  val create : ?now:bool -> unit obs -> t
  val for_cell : ?now:bool -> 'a C.t -> ('a -> unit) -> t
  val force : t -> unit
  val destroy : t -> unit
  val update : Step.t -> t -> unit
  val hold : t -> unit
  val may_hold : t option -> unit
  val unhold_all : unit -> unit
end = struct
  type 'a obs = C.untyped list * (unit -> 'a)
  let const v = [], fun () -> v
  let obs_cell c = [C.C c], fun () -> C.value c
  let app (fcs, f) (vcs, v) = List.rev_append fcs vcs, fun () -> (f ()) (v ())
  let ( $ ) = app

  type t =
    { mutable stamp : Step.t;
      mutable srcs : Srcs.t; (* sources we are registered with *)
      cells : C.untyped list;  (* cells we are observing *)
      log : unit -> unit (* logger action *) }

  let update_srcs l =
    let cells_srcs l =
      let add_cell acc (C.C c) = Srcs.union acc (C.srcs c) in
      List.fold_left add_cell Srcs.empty l.cells
    in
    let new_srcs = cells_srcs l in
    let rems = Srcs.diff l.srcs new_srcs in
    let adds = Srcs.diff new_srcs l.srcs in
    Srcs.iter (Src.rem_logr l) rems;
    Srcs.iter (Src.add_logr l) adds;
    l.srcs <- new_srcs

  let update step l =
    if step != Step.nil && step != l.stamp then begin
      l.stamp <- step;
      let rec loop step srcs_changed value_changed = function
      | [] ->
          if srcs_changed then update_srcs l;
          if value_changed then l.log ()
      | (C.C c) :: cs ->
          C.update step c;
          loop step
            (srcs_changed || C.srcs_changed c)
            (value_changed || C.value_changed c) cs
      in
      loop step false false l.cells
    end

  let force l =
    let step = Src.find_active_step Step.nil l.srcs in
    update step l;
    l.log ()

  let create ?(now = true) (cells, log) =
    let l = { stamp = Step.nil; srcs = Srcs.empty; cells; log } in
    update_srcs l;
    if now then force l;
    l

  let for_cell ?now c log = create ?now ([C.C c], fun () -> log (C.value c))
  let destroy l = Srcs.iter (Src.rem_logr l) l.srcs
  let held : t list ref = ref []
  let hold l = held := l :: !held
  let may_hold = function None -> () | Some l -> hold l
  let unhold_all () = List.iter destroy !held; held := []
end

and Step : sig
  type t
  val create : unit -> t
  val nil : t
  val delayed : t
  val srcs : t -> Srcs.t
  val add_src : t -> Src.t -> unit
  val add_delayed : t -> Src.t -> unit
  val add_cleanup : t -> (unit -> unit) -> unit
  val execute : t -> unit
  val execute_delayed : Srcs.t -> unit
end = struct
  type t =
    { mutable srcs : Srcs.t; (* sources part of the update step *)
      mutable delayed : Srcs.t; (* sources for delayed cells *)
      mutable cleanup : (unit -> unit) list (* for reseting events to None *) }

  let _create srcs = { srcs; delayed = Srcs.empty; cleanup = [] }
  let create () = _create Srcs.empty
  let nil = create ()
  let delayed = create ()
  let srcs step = step.srcs
  let add_src step src = step.srcs <- Srcs.add src step.srcs
  let add_delayed step src = step.delayed <- Srcs.add src step.delayed
  let add_cleanup step clean = step.cleanup <- clean :: step.cleanup
  let cleanup step = List.iter (fun f -> f ()) step.cleanup; step.cleanup <- []
  let already_executed () = invalid_arg "step already executed"

  let rec execute_delayed srcs =
    let update_delayed_src ds (Src.V s) =
      let c = Src.cell s in
      C.update delayed c;
      C.set_stamp c ds;
    in
    let ds = _create srcs in
    delayed.srcs <- srcs;
    Srcs.iter (update_delayed_src ds) srcs;
    execute ds

  and execute step =
    let update_src_logs src = List.iter (Logr.update step) (Src.logrs src) in
    Srcs.iter update_src_logs step.srcs;
    Srcs.iter Src.reset_stamp step.srcs;
    cleanup step;
    add_cleanup step already_executed; (* error further executes *)
    match Srcs.is_empty step.delayed with
    | true -> ()
    | false -> execute_delayed step.delayed
end

(* High-level interface *)

type 'a signal = 'a C.t
type 'a event = 'a option C.t

(* Signal and event definition always have the same structure.

   let combinator ... =
     let update step self =
       C.update step ...
       if C.srcs_changed ... then C.set_srcs self ...
       if C.value_changed ... then C.set_{instant,value} self ...
     in
     let srcs = ...
     let step = Src.find_active_step Step.nil srcs in
     let () = C.update step ... in
     let srcs = ...
     let init =
     C.create ....

   In [update], update dependencies. If dependencies sources changed
   update the cell's sources, if dependencies values changed update
   the cell's value.

   To create the cell. Get the dependency sources. Find the update
   step going on (will be Step.nil if there is none). Update the
   dependencies with the step. Get the sources again (they may have changed)
   and the needed values to create the cell.

   XXX it would be nice to see if we can simply invoke [update] for
   init, possibly with a special Step.init step on Step.nil. *)

module E = struct
  type 'a t = 'a event
  type 'a send = ?step:Step.t -> 'a -> unit

  let obs = Logr.obs_cell
  let log ?now e f =
    let wrap = function None -> () | Some v -> f v in
    Some (Logr.for_cell ?now e wrap)

  let create () =
    let src = Src.create None in
    let send ?step v =
      let step, exec = match step with
      | None -> Step.create (), true
      | Some step -> step, false
      in
      C.set_stamp (Src.cell src) step;
      C.set_instant step (Src.cell src) (Some v);
      Step.add_src step (Src.untyped src);
      if exec then Step.execute step
    in
    (Src.cell src), send

  let value = C.up_to_date_value
  let never = (* XXX *) Obj.magic @@ C.const None
  let bind e f =
    let step = Src.find_active_step Step.nil (C.srcs e) in
    let () = C.update step e in
    let current = match C.value e with None -> never | Some curr -> f curr in
    let current = ref current in
    let update step self =
      C.update step e;
      match C.value e with
      | None ->
          C.update step !current;
          if C.(srcs_changed e || srcs_changed !current) then
            C.set_srcs self (Srcs.union (C.srcs e) (C.srcs !current));
          C.set_instant step self (C.value !current)
      | Some curr ->
          current := f curr;
          C.update step !current;
          C.set_srcs self (Srcs.union (C.srcs e) (C.srcs !current));
          C.set_instant step self (C.value !current)
    in
    let step = Src.find_active_step step (C.srcs !current) in
    let () = C.update step !current in
    let srcs = Srcs.union (C.srcs e) (C.srcs !current) in
    let init = C.value !current in
    C.create_instant ~step ~srcs init ~update

  let join ee = bind ee (fun e -> e)
  let swap es =
    let step = Src.find_active_step Step.nil (C.srcs es) in
    let () = C.update step es in
    let current = ref (C.value es) in
    let update step self =
      C.update step es;
      begin match C.value_changed es with
      | false ->
          C.update step !current;
          if C.(srcs_changed es || srcs_changed !current)
          then C.set_srcs self (Srcs.union (C.srcs es) (C.srcs !current))
      | true ->
          current := C.value es;
          C.update step !current;
          C.set_srcs self (Srcs.union (C.srcs es) (C.srcs !current));
      end;
      C.set_instant step self (C.value !current)
    in
    let step = Src.find_active_step step (C.srcs !current) in
    let () = C.update step !current in
    let srcs = Srcs.union (C.srcs es) (C.srcs !current) in
    let init = C.value !current in
    C.create_instant ~step ~srcs init ~update

  let map f e =
    let map f = function None -> None | Some v -> Some (f v) in
    let update step self =
      C.update step e;
      if C.srcs_changed e then C.set_srcs self (C.srcs e);
      C.set_instant step self (map f (C.value e))
    in
    let step = Src.find_active_step Step.nil (C.srcs e) in
    let () = C.update step e in
    C.create_instant ~step ~srcs:(C.srcs e) (map f (C.value e)) ~update

  let stamp e v =
    let stamp = function None -> None | Some _ -> Some v in
    let update step self =
      C.update step e;
      if C.srcs_changed e then C.set_srcs self (C.srcs e);
      C.set_instant step self (stamp (C.value e))
    in
    let step = Src.find_active_step Step.nil (C.srcs e) in
    let () = C.update step e in
    let init = stamp (C.value e) in
    C.create_instant ~step ~srcs:(C.srcs e) init ~update

  let filter f e =
    let filter f = function
    | None -> None
    | Some v as occ when f v -> occ
    | Some _ -> None
    in
    let update step self =
      C.update step e;
      if C.srcs_changed e then C.set_srcs self (C.srcs e);
      C.set_instant step self (filter f (C.value e))
    in
    let step = Src.find_active_step Step.nil (C.srcs e) in
    let () = C.update step e in
    let init = filter f (C.value e) in
    C.create_instant ~step ~srcs:(C.srcs e) init ~update

  let filter_map f e =
    let filter_map f = function None -> None | Some v -> f v in
    let update step self =
      C.update step e;
      if C.srcs_changed e then C.set_srcs self (C.srcs e);
      C.set_instant step self (filter_map f (C.value e))
    in
    let step = Src.find_active_step Step.nil (C.srcs e) in
    let () = C.update step e in
    let init = filter_map f (C.value e) in
    C.create_instant ~step ~srcs:(C.srcs e) init ~update

  let select es =
    let add_srcs acc e = Srcs.union acc (C.srcs e) in
    let or_srcs_changed acc e = acc || C.srcs_changed e in
    let update step self =
      List.iter (C.update step) es;
      let srcs_changed = List.fold_left or_srcs_changed false es in
      if srcs_changed
      then C.set_srcs self (List.fold_left add_srcs Srcs.empty es);
      let v = match List.find (fun e -> C.value e <> None) es with
      | exception Not_found -> None | e -> C.value e
      in
      C.set_instant step self v
    in
    let find_step step e = Src.find_active_step step (C.srcs e) in
    let step = List.fold_left find_step Step.nil es in
    let () = List.iter (C.update step) es in
    let init = match List.find (fun e -> C.value e <> None) es with
    | exception Not_found -> None | e -> C.value e
    in
    let srcs = List.fold_left add_srcs Srcs.empty es in
    C.create_instant ~step ~srcs init ~update

  let accum acc e =
    let acc = ref acc in
    let accum = function None -> None | Some f -> acc := f !acc; Some !acc in
    let update step self =
      C.update step e;
      if C.srcs_changed e then C.set_srcs self (C.srcs e);
      C.set_instant step self (accum (C.value e))
    in
    let step = Src.find_active_step Step.nil (C.srcs e) in
    let () = C.update step e in
    let init = accum (C.value e) in
    C.create_instant ~step ~srcs:(C.srcs e) init ~update

  let until ?(limit = false) ~next e =
    let nop step self = () in
    let update step self =
      C.(update step next; update step e);
      match C.value next with
      | None ->
          if C.(srcs_changed next || srcs_changed e)
          then C.set_srcs self (Srcs.union (C.srcs next) (C.srcs e));
          C.set_instant step self (C.value e)
      | Some _ ->
          C.set_srcs self Srcs.empty;
          C.set_update self nop;
          C.set_instant step self (if limit then C.value e else None)
    in
    let step = Src.find_active_step Step.nil (C.srcs next) in
    let step = Src.find_active_step step (C.srcs e) in
    let () = C.(update step next; update step e) in
    match C.value next with
    | None ->
        let srcs = Srcs.union (C.srcs next) (C.srcs e) in
        C.create_instant ~step ~srcs (C.value e) ~update
    | Some _ ->
        let init = if limit then C.value e else None in
        C.create_instant ~step ~srcs:Srcs.empty init ~update:nop

  let follow e ~on =
    (* FIXME rewrite combinators with this style.
       FIXME determine why we don't simply call update for init in general *)
    let deps_srcs e on = Srcs.union (C.srcs e) (C.srcs on) in
    let deps_srcs_changed e on = C.(srcs_changed e || srcs_changed on) in
    let update_deps step e on = C.(update step e; update step on) in
    let follow e on = match e with Some _ as o when on -> o | _ -> None in
    let update step self =
      update_deps step e on;
      if deps_srcs_changed e on then C.set_srcs self (deps_srcs e on);
      C.set_instant step self (follow (C.value e) (C.value on))
    in
    let step = Src.find_active_step Step.nil (C.srcs e) in
    let step = Src.find_active_step step (C.srcs on) in
    let () = update_deps step e on in
    let init = follow (C.value e) (C.value on) in
    C.create_instant ~step ~srcs:(deps_srcs e on) init ~update

  let defer e = C.defer None e
  let fix ef = C.fix None ef

  module Option = struct
    let on_some e = filter_map (fun x -> x) e
    let some e = map (fun v -> Some v) e
    let value e ~default =
      let update step self =
        C.update step e;
        if C.srcs_changed e then C.set_srcs self (C.srcs e);
        let occ = match C.value e with
        | None -> None
        | Some (Some _ as v) -> v
        | Some None -> C.update step default; Some (C.value default)
        in
        C.set_instant step self occ
      in
      let step = Src.find_active_step Step.nil (C.srcs e) in
      let () = C.update step e; C.update step default in
      let init = match C.value e with
      | None -> None
      | Some (Some _ as v) -> v
      | Some None -> Some (C.value default)
      in
      C.create_instant ~step ~srcs:(C.srcs e) init ~update

    let get e =
      map (function Some v -> v | None -> invalid_arg "option is None") e

    let bind e f = map (function None -> None | Some v -> f v) e
    let join e = map (function Some (Some _ as o) -> o | _ -> None) e
    let is_none e = map (function None -> true | Some _ -> false) e
    let is_some e = map (function None -> false | Some _ -> true) e
    let map f e = map (function None -> None | Some v -> Some (f v)) e
  end

  module Pair = struct
    let fst e = map fst e
    let snd e = map snd e
    let v e0 e1 =
      let update step self =
        C.(update step e0; update step e1);
        if C.(srcs_changed e0 || srcs_changed e1)
        then C.set_srcs self (Srcs.union (C.srcs e0) (C.srcs e1));
        let occ = match C.value e0, C.value e1 with
        | Some v0, Some v1 -> Some (v0, v1)
        | _ -> None
        in
        C.set_instant step self occ
      in
      let step = Src.find_active_step Step.nil (C.srcs e0) in
      let step = Src.find_active_step step (C.srcs e1) in
      let srcs = Srcs.union (C.srcs e0) (C.srcs e1) in
      let init = match C.value e0, C.value e1 with
      | Some v0, Some v1 -> Some (v0, v1)
      | _ -> None
      in
      C.create_instant ~step ~srcs init ~update
  end

  let dump_src_ids = C.dump_src_ids
end

module S = struct
  type 'a t = 'a signal
  type 'a set = ?step:Step.t -> 'a -> unit

  let log = Logr.for_cell
  let obs = Logr.obs_cell
  let eq = C.eq
  let with_eq = C.with_eq
  let create ?eq v =
    let src = Src.create ?eq v in
    let set ?step v =
      let step, exec = match step with
      | None -> Step.create (), true
      | Some step -> step, false
      in
      let cell = Src.cell src in
      if C.src_update step cell v
      then Step.add_src step (Src.untyped src);
      if exec then Step.execute step
    in
    Src.cell src, set

  let value = C.up_to_date_value
  let rough_value = C.value
  let const = C.const
  let bind v f =
    let step = Src.find_active_step Step.nil (C.srcs v) in
    let () = C.update step v in
    let current = ref (f (C.value v)) in
    let update step self =
      C.update step v;
      match C.value_changed v with
      | false ->
          C.update step !current;
          if C.(srcs_changed v || srcs_changed !current) then
            C.set_srcs self (Srcs.union (C.srcs v) (C.srcs !current));
          if C.value_changed !current then C.set_value self (C.value !current)
      | true ->
          current := f (C.value v);
          C.update step !current;
          C.set_eq self (C.eq !current);
          C.set_srcs self (Srcs.union (C.srcs v) (C.srcs !current));
          C.set_value self (C.value !current)
    in
    let step = Src.find_active_step step (C.srcs !current) in
    let () = C.update step !current in
    let srcs = Srcs.union (C.srcs v) (C.srcs !current) in
    let init = C.value !current in
    C.create ~eq:(C.eq !current) ~step ~srcs init ~update

  let hold ?eq i e =
    let update step self =
      C.update step e;
      if C.(srcs_changed e) then C.set_srcs self (C.srcs e);
      match C.value e with
      | None -> ()
      | Some v -> C.set_value self v
    in
    let step = Src.find_active_step Step.nil (C.srcs e) in
    let () = C.update step e in
    let init = match C.value e with None -> i | Some v -> v in
    C.create ?eq ~step ~srcs:(C.srcs e) init ~update

  let join ss = bind ss (fun s -> s)
  let swap s se = join (hold ~eq:( == ) s se)
  let changes s =
    let update step self =
      C.update step s;
      if C.srcs_changed s then C.set_srcs self (C.srcs s);
      if C.value_changed s then C.set_instant step self (Some (C.value s))
    in
    let step = Src.find_active_step Step.nil (C.srcs s) in
    let () = C.update step s in
    (* NB: 0 - dt doesn't exist so this is always None *)
    C.create_instant ~step ~srcs:(C.srcs s) None ~update

  let sample s ~on f =
    let update step self =
      C.(update step on; update step s);
      if C.(srcs_changed on || srcs_changed s)
      then C.set_srcs self (Srcs.union (C.srcs s) (C.srcs on));
      match C.value on with
      | None -> ()
      | Some v -> C.set_instant step self (Some (f (C.value s) v))
    in
    let step = Src.find_active_step Step.nil (C.srcs s) in
    let step = Src.find_active_step step (C.srcs on) in
    let () = C.(update step on; update step s) in
    let srcs = Srcs.union (C.srcs s) (C.srcs on) in
    let init = match C.value on with
    | None -> None
    | Some v -> Some (f (C.value s) v)
    in
    C.create_instant ~step ~srcs init ~update

  let sample_filter s ~on f = E.Option.on_some (sample s ~on f)
  let snapshot s ~on = sample s ~on (fun v _ -> v)

  let map ?eq f v =
    let update step self =
      C.update step v;
      if C.srcs_changed v then C.set_srcs self (C.srcs v);
      if C.value_changed v then C.set_value self (f (C.value v))
    in
    let step = Src.find_active_step Step.nil (C.srcs v) in
    let () = C.update step v in
    C.create ?eq ~step ~srcs:(C.srcs v) (f (C.value v)) ~update

  let app ?eq f v =
    let update step self =
      C.(update step f; update step v);
      if C.(srcs_changed f || srcs_changed v) then
        C.set_srcs self (Srcs.union (C.srcs f) (C.srcs v));
      if C.(value_changed f || value_changed v) then
        C.set_value self ((C.value f) (C.value v))
    in
    let step = Src.find_active_step Step.nil (C.srcs f) in
    let step = Src.find_active_step step (C.srcs v) in
    let () = C.update step f; C.update step v in
    let srcs = Srcs.union (C.srcs f) (C.srcs v) in
    let init = (C.value f) (C.value v) in
    C.create ?eq ~step ~srcs init ~update

  let accum ?eq i e = hold ?eq i (E.accum i e)
  let until ?(limit = false) ?init ~next s =
    let nop step self = () in
    let update step self =
      C.(update step next; update step s);
      match C.value next with
      | None ->
          if C.(srcs_changed next || srcs_changed s)
          then C.set_srcs self (Srcs.union (C.srcs next) (C.srcs s));
          C.set_value self (C.value s)
      | Some _ ->
          C.set_srcs self Srcs.empty;
          C.set_update self nop;
          if limit then C.set_value self (C.value s) else ()
    in
    let step = Src.find_active_step Step.nil (C.srcs next) in
    let step = Src.find_active_step step (C.srcs s) in
    let () = C.(update step next; update step s) in
    match C.value next with
    | None ->
        let srcs = Srcs.union (C.srcs next) (C.srcs s) in
        C.create ~eq:(eq s) ~step ~srcs (C.value s) ~update
    | Some _ ->
        let init = match init with None -> C.value s | Some i -> i in
        C.create ~eq:(eq s) ~step ~srcs:Srcs.empty init ~update:nop

  let follow ?init s ~on =
    let deps_srcs s on = Srcs.union (C.srcs s) (C.srcs on) in
    let deps_srcs_changed s on = C.(srcs_changed s || srcs_changed on) in
    let update_deps step s on = C.(update step s; update step on) in
    let update step self =
      update_deps step s on;
      if deps_srcs_changed s on then C.set_srcs self (deps_srcs s on);
      if C.value on then C.set_value self (C.value s)
    in
    let step = Src.find_active_step Step.nil (C.srcs s) in
    let step = Src.find_active_step step (C.srcs on) in
    let () = update_deps step s on in
    let init = match init with None -> (C.value s) | Some i -> i in
    C.create ~eq:(eq s) ~step ~srcs:(deps_srcs s on) init ~update

  let delay = C.delay
  let defer ?init s =
    let init = match init with
    | Some init -> init
    | None ->
        let step = Src.find_active_step Step.nil (C.srcs s) in
        let () = C.update step s in
        C.value s
    in
    C.defer init s

  let fix = C.fix
  let l1 ?eq f x = map ?eq f x
  let l2 ?eq f x y =
    let update step self =
      C.(update step x; update step y);
      if C.(srcs_changed x || srcs_changed y)
      then C.set_srcs self (Srcs.union (C.srcs x) (C.srcs y));
      if C.(value_changed x || value_changed y)
      then C.set_value self (f (C.value x) (C.value y))
    in
    let step = Src.find_active_step Step.nil (C.srcs x) in
    let step = Src.find_active_step step (C.srcs y) in
    let () = C.(update step x; update step y) in
    let srcs = Srcs.union (C.srcs x) (C.srcs y) in
    let init = f (C.value x) (C.value y) in
    C.create ?eq ~step ~srcs init ~update

  let l3 ?eq f x y z =
    let srcs_union x y z =
      Srcs.union (C.srcs x) (Srcs.union (C.srcs y) (C.srcs z))
    in
    let update step self =
      C.(update step x; update step y; update step z);
      if C.(srcs_changed x || srcs_changed y || srcs_changed z)
      then C.set_srcs self (srcs_union x y z);
      if C.(value_changed x || value_changed y || value_changed z)
      then C.set_value self (f (C.value x) (C.value y) (C.value z))
    in
    let step = Src.find_active_step Step.nil (C.srcs x) in
    let step = Src.find_active_step step (C.srcs y) in
    let step = Src.find_active_step step (C.srcs z) in
    let () = C.(update step x; update step y; update step z) in
    let srcs = srcs_union x y z in
    let init = f (C.value x) (C.value y) (C.value z) in
    C.create ?eq ~step ~srcs init ~update

  module Bool = struct
    let eq : bool -> bool -> bool = ( = )
    let false' = const false
    let true' = const true
    let not s = map ~eq not s
    let ( && ) = l2 ( && )
    let ( || ) = l2 ( || )
    let edge s = changes s
    let edge_detect edge s =
      let update step self =
        C.update step s;
        if C.srcs_changed s then C.set_srcs self (C.srcs s);
        if Stdlib.( && ) (C.value_changed s) (C.value s = edge)
        then C.set_instant step self (Some ())
      in
      let step = Src.find_active_step Step.nil (C.srcs s) in
      let () = C.update step s in
      C.create_instant ~step ~srcs:(C.srcs s) None ~update

    let rise s = edge_detect true s
    let fall s = edge_detect false s
    let flip ~init e =
      let update step self =
        C.update step e;
        if C.srcs_changed e then C.set_srcs self (C.srcs e);
        match C.value e with
        | None -> ()
        | Some _ -> C.set_value self (Stdlib.not (C.value self))
      in
      let step = Src.find_active_step Step.nil (C.srcs e) in
      let () = C.update step e in
      let init = match C.value e with
      | Some _ -> Stdlib.not init
      | None -> init
      in
      C.create ~eq ~step ~srcs:(C.srcs e) init ~update
  end

  module Option = struct
    let _eq eq = fun v0 v1 -> match v0, v1 with
    | Some v0, Some v1 -> eq v0 v1
    | None, None -> true
    | _, _ -> false

    let none = (* XXX *) Obj.magic @@ (const None)
    let some s = map ~eq:(_eq (eq s)) (fun v -> Some v) s

    let hold_value i s =
      let update step self =
        C.update step s;
        if (C.srcs_changed s) then C.set_srcs self (C.srcs s);
        match C.value s with None -> () | Some v -> C.set_value self v
      in
      let eq v v' = C.eq s (Some v) (Some v') in
      let step = Src.find_active_step Step.nil (C.srcs s) in
      let () = C.update step s in
      let init = match C.value s with None -> i | Some v -> v in
      C.create ~eq ~step ~srcs:(C.srcs s) init ~update

    let value s ~default =
      let update step self =
        C.update step default; C.update step s;
        if C.(srcs_changed default || C.srcs_changed s)
        then C.set_srcs self (Srcs.union (C.srcs default) (C.srcs s));
        if (C.value_changed default || C.value_changed s)
        then match C.value s with
        | None -> C.set_value self (C.value default)
        | Some v -> C.set_value self v
      in
      let step = Src.find_active_step Step.nil (C.srcs default) in
      let step = Src.find_active_step step (C.srcs s) in
      let () = C.(update step default; update step s) in
      let init = match C.value s with None -> C.value default | Some v -> v in
      let srcs = Srcs.union (C.srcs default) (C.srcs s) in
      C.create ~eq:(eq default) ~step ~srcs init ~update

    let get ?eq s =
      map ?eq (function Some v -> v | None -> invalid_arg "option is None") s

    let bind ?eq s f = map ?eq (function None -> None | Some v -> f v) s
    let join ?eq s = map ?eq (function Some (Some _ as o) -> o | _ -> None) s
    let is_none s = map ~eq:Bool.eq (function None -> true | Some _ -> false) s
    let is_some s = map ~eq:Bool.eq (function None -> false | Some _ -> true) s
    let map ?eq f s = map ?eq (function None -> None | Some v -> Some (f v)) s
    let eq = _eq
  end

  module Pair = struct
    let fst ?eq s = map ?eq fst s
    let snd ?eq s = map ?eq snd s
    let v s0 s1 = l2 (fun x y -> (x, y)) s0 s1
  end

  let dump_src_ids = C.dump_src_ids
end

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
