(*---------------------------------------------------------------------------
   Copyright (c) 2018 The note programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The implementation respects the framework given at
   http://todomvc.com/. Notably the CSS file and markup
   structure. Some things could be implemented differently, organized
   in a more natural, modular and generic manner by lifting this
   restriction.

   The UX was also kept as defined by the backbone.js reference
   implementation -- but it's not always good. Here are a few things
   that could be improved:

   1. The footer should not be placed at the bottom of the todo list.
      First this means you have to scroll if you have many
      todos. Second it moves away from your pointer when you click on
      a filter which is very annoying when you want to quickly switch
      between filters.

   2. Operating on the data when filters are active is a bit
      confusing. For example adding a new todo when the filter is
      "Completed" should disable the filter otherwise the user has the
      feeling no new item is being added. Toggling completeness is
      equally confusing (though a bit of animation could do here).

   3. The "toggle all" button look and behaviour is a bit confusing. *)

open Brr
open Brr_io
open Note
open Note_brr
open Note_brr_kit

(* Model *)

module Todo : sig
  type t
  val v : Jstr.t -> t
  val task : t -> Jstr.t
  val done' : t -> bool
  val with_task : Jstr.t -> t -> t
  val with_done : bool -> t -> t
  val to_json : t -> Json.t
  val of_json : Json.t -> t
end = struct
  type t = { task : Jstr.t; done' : bool }
  let v task = { task; done' = false }
  let task t = t.task
  let done' t = t.done'
  let with_task task t = { t with task }
  let with_done done' t = { t with done' }
  let to_json t = Jv.(obj [|"task", of_jstr t.task; "done", of_bool t.done' |])
  let of_json j = { task = Jv.Jstr.get j "task"; done' = Jv.Bool.get j "done" }
end

module Todos : sig
  type t
  val empty : t
  val is_empty : t -> bool
  val to_list : t -> Todo.t list
  val count : t -> int
  val add : Todo.t -> t -> t
  val rem : Todo.t -> t -> t
  val replace : Todo.t -> by:Todo.t -> t -> t
  val map : (Todo.t -> Todo.t) -> t -> t
  val fold : (Todo.t -> 'a -> 'a) -> t -> 'a -> 'a
  val filter : (Todo.t -> bool) -> t -> t
  val for_all : (Todo.t -> bool) -> t -> bool
  val exists : (Todo.t -> bool) -> t -> bool
  val to_json : t -> Json.t
  val of_json : Json.t -> t
end = struct
  type t = Todo.t list
  let empty = []
  let is_empty ts = ts = empty
  let to_list ts = ts
  let count ts = List.length ts
  let update upd t ts =
    let upd t acc = match upd t with None -> acc | Some v -> (v :: acc) in
    let rec loop acc = function
    | [] -> List.rev acc
    | t' :: ts when t == t' -> List.rev_append (upd t acc) ts
    | t' :: ts -> loop (t' :: acc) ts
    in
    loop [] ts

  let add t ts = t :: ts
  let rem = update (fun _ -> None)
  let replace t ~by = update (fun _ -> Some by) t
  let map f ts = List.(rev @@ rev_map f ts)
  let fold f ts acc = List.fold_left (fun acc t -> f t acc) acc ts
  let filter sat = List.filter sat
  let for_all sat = List.for_all sat
  let exists sat = List.exists sat
  let to_json ts = Jv.of_list Todo.to_json ts
  let of_json j = Jv.to_list Todo.of_json j
end

(* Model actions *)

type add_action = [ `Add_todo of Jstr.t ]
type bulk_action = [ `All_done of bool | `Rem_done ]
type edit_action =
[ `Set_task of Jstr.t * Todo.t
| `Set_done of bool * Todo.t
| `Rem_todo of Todo.t ]

type action = [ add_action | bulk_action | edit_action ]

let do_action : action -> Todos.t -> Todos.t = function
| `Add_todo task -> Todos.add (Todo.v task)
| `Set_task (task, todo) -> Todos.replace todo ~by:(Todo.with_task task todo)
| `Set_done (d, todo) -> Todos.replace todo ~by:(Todo.with_done d todo)
| `Rem_todo todo -> Todos.rem todo
| `All_done d -> Todos.map (Todo.with_done d)
| `Rem_done -> Todos.filter (fun t -> not (Todo.done' t))

(* Persisting FIXME make that versioned (like the old Brr_note_legacy.Store)
   and easier. *)

let state_key = Jstr.v "brr-todomvc-state"
let save_state ts =
  let s = Storage.local G.window in
  (Storage.set_item s state_key (Json.encode (Todos.to_json ts)))
  |> Console.log_if_error ~use:()

let load_state () =
  let s = Storage.local G.window in
  match Storage.get_item s state_key with
  | None -> Todos.empty
  | Some j ->
      (Result.map Todos.of_json (Json.decode j))
      |> Console.log_if_error ~use:Todos.empty

(* Rendering & interaction *)

let el_def_display : El.t -> bool signal -> unit =
  (* Would maybe be better to do this via CSS classes *)
  let none = Jstr.v "none" and show = Jstr.empty in
  let bool_to_display = function true -> show | false -> none in
  fun el bool ->
    Elr.def_inline_style El.Style.display (S.map bool_to_display bool) el

let add_todo : unit -> [> add_action] event * El.t =
fun () ->
  let p = Jstr.v "What needs to be done ?" in
  let typ = At.type' (Jstr.v "text") in
  let at = At.[typ; class' (Jstr.v "new-todo"); autofocus; placeholder p] in
  let i = El.input ~at () in
  let keydown = Ev.keydown in
  let return = E.filter (Key.equal `Return) (Evr.on_el keydown Key.of_ev i) in
  let input = E.map (fun _ -> Jstr.trim @@ El.prop El.Prop.value i) return in
  let add_todo = input |> E.filter_map @@ fun v -> match Jstr.is_empty v with
  | true -> None
  | false -> Some (`Add_todo v)
  in
  let clear = E.stamp add_todo Jstr.empty in
  let () = Elr.set_prop El.Prop.value i ~on:clear in
  add_todo, i

let toggle_all : set:bool signal -> [> bulk_action ] event * El.t =
fun ~set ->
  let tid = Jstr.v "toggle-all" in
  let typ = At.type' (Jstr.v "checkbox") in
  let i = El.input ~at:At.[typ; class' tid; id tid] () in
  let () = Elr.def_prop El.Prop.checked set i in
  let click = Evr.on_el Ev.click Evr.unit i in
  let toggle =
    E.map (fun _ -> `All_done (El.prop El.Prop.checked i)) click
  in
  let label = [El.txt (Jstr.v "Mark all as complete")] in
  let label = El.label ~at:At.[for' tid] label in
  toggle, El.div [i; label]

let items_left : count:int signal -> El.t =
fun ~count ->
  let count_msg = function
  | 0 -> Jstr.v "0 items left"
  | 1 -> Jstr.v "1 item left"
  | n -> Jstr.(of_int n + v " items left")
  in
  let span = El.span ~at:At.[class' (Jstr.v "todo-count")] [] in
  let msg = S.map (fun c -> [El.txt (count_msg c)]) count in
  let () = Elr.def_children span msg in
  span

type filter = [ `All | `Todo | `Done ]
let filters : unit -> filter signal * El.t =
fun () ->
  let fragment _ = Uri.fragment (Window.location G.window) in
  let hashchange =
    Evr.on_target Ev.hashchange fragment (Window.as_target G.window)
  in
  let parse_frag frag = match Jstr.to_string frag with
  | "/active" -> `Todo | "/completed" -> `Done | v -> `All
  in
  let init_filter = parse_frag (fragment ()) in
  let filter_li frag name =
    let a = El.(a ~at:At.[href Jstr.(v "#" + frag)] [El.txt (Jstr.v name)]) in
    let sel = parse_frag frag = init_filter in
    let selected = S.hold sel (E.map (Jstr.equal frag) hashchange) in
    let () = Elr.def_class (Jstr.v "selected") selected a in
    El.li [a]
  in
  let all = filter_li (Jstr.v "/") "All" in
  let todo = filter_li (Jstr.v "/active") "Active" in
  let done' = filter_li (Jstr.v "/completed") "Completed" in
  let filter = S.hold init_filter (E.map parse_frag hashchange) in
  filter, El.ul ~at:At.[class' (Jstr.v "filters")] [all; todo; done']

let string_editor :
  Jstr.t -> on:'a event -> bool event * Jstr.t event * El.t =
fun s ~on ->
  let ed = El.input ~at:At.[class' (Jstr.v "edit"); value s] () in
  let keys = Evr.on_el Ev.keydown Key.of_ev ed in
  let edited = E.filter (Key.equal `Return) keys in
  let undo = E.filter (Key.equal `Escape) keys in
  let start_edit = E.stamp on true in
  let stop_edit = E.stamp (E.select [edited; undo]) false in
  let editing = E.select [start_edit; stop_edit] in
  let str = E.map (fun _ -> El.prop El.Prop.value ed) edited in
  let () = Elr.set_prop El.Prop.value ~on:(E.map (fun _ -> s) undo) ed in
  let () = Elr.set_has_focus ~on:start_edit ed in
  let () = Elr.call (fun _ e -> El.select_text e) ~on:start_edit ed in
  editing, str, ed

let bool_editor : bool -> bool event * El.t =
fun b ->
  let at = At.[type' (Jstr.v "checkbox"); class' (Jstr.v "toggle")] in
  let at = At.(if' b checked) :: at in
  let el = El.input ~at () in
  let click = Evr.on_el Ev.click Evr.unit el in
  let toggle = E.map (fun () -> El.prop El.Prop.checked el) click in
  toggle, el

let todo_item : Todo.t -> [> edit_action ] event * El.t =
fun todo ->
  let done' = Todo.done' todo in
  let task = Todo.task todo in
  let set_done, done_editor = bool_editor done' in
  let set_done = E.map (fun d -> `Set_done (d, todo)) set_done in
  let rem_but = El.button ~at:At.[class' (Jstr.v "destroy")] [] in
  let rem = Evr.on_el Ev.click (Evr.stamp (`Rem_todo todo)) rem_but in
  let label = El.label [El.txt task] in
  let editing, edited, ed =
    string_editor task ~on:(Evr.on_el Ev.dblclick Evr.unit label)
  in
  let edit = edited |> E.filter_map @@ fun v ->
    let v = Jstr.trim v in
    if Jstr.is_empty v then Some (`Rem_todo todo) else
    if not (Jstr.equal v task) then Some (`Set_task (v, todo)) else None
  in
  let div_at = At.[class' (Jstr.v "view")] in
  let div = El.div ~at:div_at [done_editor; label; rem_but] in
  let li_at = At.[if' done' (class' (Jstr.v "completed"))] in
  let li = El.li ~at:li_at [div; ed] in
  let () = Elr.set_class (Jstr.v "editing") ~on:editing li in
  E.select [edit; rem; set_done], li

let todo_list :
  Todos.t signal -> filter:filter signal -> [> edit_action ] event * El.t =
fun ts ~filter ->
  let add filter t (es, is as acc) = match filter with
  | `Todo when Todo.done' t -> acc
  | `Done when not (Todo.done' t) -> acc
  | _ -> let e, i = todo_item t in (e :: es, i :: is)
  in
  let add_todos ts filter = Todos.fold (add filter) ts ([], []) in
  let items = S.l2 ~eq:( == ) add_todos ts filter in
  let act = E.swap @@ S.map ~eq:( == ) (fun (evs, _) -> E.select evs) items in
  let items = S.map snd items in
  let ul = El.ul ~at:At.[class' (Jstr.v "todo-list")] [] in
  let () = Elr.def_children ul items in
  act, ul

let header () =
  let add, field = add_todo () in
  let at = At.[class' (Jstr.v "header")] in
  add, El.header ~at [El.h1 [El.txt (Jstr.v "todos")]; field]

let footer ~todos =
  let is_todo t = not (Todo.done' t) in
  let has_done = S.map (Todos.exists Todo.done') todos in
  let todo_left ts = List.(length @@ filter is_todo (Todos.to_list ts)) in
  let left_el = items_left ~count:(S.map todo_left todos) in
  let filter, fs_el = filters () in
  let rem_done, rem_el =
    let at = At.[class' (Jstr.v "clear-completed")] in
    let b = El.button ~at [El.txt (Jstr.v "Clear completed")] in
    let () = el_def_display b has_done in
    let rem_done = Evr.on_el Ev.click (Evr.stamp `Rem_done) b in
    rem_done, b
  in
  let at = At.[class' (Jstr.v "footer")] in
  let ft = El.footer ~at [left_el;fs_el;rem_el] in
  let display ts = not @@ Todos.is_empty ts in
  let () = el_def_display ft (S.map display todos) in
  filter, rem_done, ft

let main ~add_todo ~rem_done ~todos ~filter =
  let toggle_set = todos |> S.map @@ fun ts ->
    Todos.(not (is_empty ts) && for_all Todo.done' ts)
  in
  let toggle_all, toggle_el = toggle_all ~set:toggle_set in
  let edit, items = todo_list todos ~filter in
  let at = At.[class' (Jstr.v "main")] in
  let sec = El.section ~at [toggle_el; items] in
  let display ts = not @@ Todos.is_empty ts in
  let () = el_def_display sec (S.map display todos) in
  E.select [add_todo; rem_done; edit; toggle_all], sec

let ui : todos:Todos.t -> (Todos.t signal * El.t list) =
fun ~todos ->
  let def todos =
    let add_todo, header = header () in
    let filter, rem_done, footer = footer ~todos in
    let action, main = main ~add_todo ~rem_done ~todos ~filter in
    let do_action = E.map do_action action in
    let todos' = S.accum (S.value todos) do_action in
    todos', (todos', [header; main; footer])
  in
  S.fix todos def

let main () =
  let id = Jstr.v "app" in
  match Document.find_el_by_id G.document id with
  | None -> Console.(error [str "No element with id '%s' found"; id])
  | Some el ->
      let todos, children = ui ~todos:(load_state ()) in
      Logr.(hold @@ S.log todos save_state);
      El.set_children el children

let () = main ()
