open B0_kit.V000

(* OCaml library names *)

let unix = B0_ocaml.libname "unix"
let brr = B0_ocaml.libname "brr"

let note = B0_ocaml.libname "note"
let note_brr = B0_ocaml.libname "note.brr"

(* Libraries *)

let note_lib =
  let srcs = Fpath.[ `File (v "src/note.mli"); `File (v "src/note.ml") ] in
  let requires = [] in
  B0_ocaml.lib note ~doc:"The note library" ~srcs ~requires

let note_brr_lib =
  let srcs = Fpath.[ `Dir (v "src/brr") ] in
  let requires = [brr; note] in
  B0_ocaml.lib note_brr ~doc:"Brr Note support" ~srcs ~requires

(* Tests *)

let test_exe ?(requires = []) src ~doc =
  let src = Fpath.v src in
  let srcs = Fpath.[`File src] in
  let meta = B0_meta.(empty |> tag test) in
  let requires = note :: requires in
  B0_ocaml.exe (Fpath.basename ~no_ext:true src) ~srcs ~doc ~meta ~requires

let test = test_exe "test/test.ml" ~doc:"Test suite"
let clock =
  test_exe "test/clock.ml" ~doc:"Reactive clock example" ~requires:[unix]

let test_assets = Fpath.[ `File (v "test/base.css") ]
let test_jsoo ?(requires = [brr]) n ~doc =
  let srcs = `File (Fpath.v (Fmt.str "test/%s.ml" n)) :: test_assets in
  let meta = B0_jsoo.meta ~requires () in
  B0_jsoo.web n ~doc ~srcs ~meta

let test_jsoo_module ?doc top m requires  =
  let test = Fmt.str "test_%s" (String.Ascii.uncapitalize m) in
  let doc = Fmt.str "Test %s.%s module" top m in
  let srcs = `File (Fpath.v (Fmt.str "test/%s.ml" test)) :: test_assets in
  let comp = Cmd.(arg "--pretty") in
  let meta = B0_jsoo.meta ~requires ~comp () in
  B0_jsoo.web test ~doc ~srcs ~meta

let test_key = test_jsoo_module "Note_brr_kit" "Key" [brr; note; note_brr]
let test_mouse = test_jsoo_module "Note_brr_kit" "Mouse" [brr; note; note_brr]
let test_mutobs =
  let doc = "Test use of MutationObservers by Brr_note" in
  test_jsoo "test_mutobs" ~doc

let test_leak =
  let requires = [brr; note; note_brr] in
  test_jsoo "test_leak" ~requires ~doc:"Tests reactive DOM gc strategy"

let todomvc =
  let srcs = Fpath.[ `File (v "test/todomvc.ml");
                     `File (v "test/todomvc.html"); ]
  in
  let requires = [brr; note; note_brr;] in
  let meta = B0_jsoo.meta ~requires () in
  B0_jsoo.web "todomvc" ~doc:"TodoMVC app" ~srcs ~meta

(* Packs *)

let default =
  let meta =
    let open B0_meta in
    empty
    |> tag B0_opam.tag
    |> add authors ["The note programmers"]
    |> add maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> add homepage "https://erratique.ch/software/note"
    |> add online_doc "https://erratique.ch/software/note/doc/"
    |> add licenses ["ISC"]
    |> add repo "git+https://erratique.ch/repos/note.git"
    |> add issues "https://github.com/dbuenzli/note/issues"
    |> add description_tags
      ["reactive"; "declarative"; "signal"; "event"; "frp"; "org:erratique";
       "browser"; ]
    |> add B0_opam.Meta.depends
      [ "ocaml", {|>= "4.08.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|};
      ]
    |> add B0_opam.Meta.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"
          "--with-brr" "%{brr:installed}%"]]|}
    |> add B0_opam.Meta.depopts ["brr", ""]
    |> add B0_opam.Meta.conflicts [ "brr", {|< "0.0.6"|}]
  in
  B0_pack.v "default" ~doc:"note package" ~meta ~locked:true @@
  B0_unit.list ()
