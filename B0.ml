open B0_kit.V000
open B00_std

(* OCaml library names *)

let note = B0_ocaml.libname "note"
let unix = B0_ocaml.libname "unix"

(* Libraries *)

let note_lib =
  let srcs = Fpath.[ `File (v "src/note.mli"); `File (v "src/note.ml") ] in
  let requires = [] in
  B0_ocaml.lib note ~doc:"The note library" ~srcs ~requires

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
      ["reactive"; "declarative"; "signal"; "event"; "frp"; "org:erratique"]
    |> add B0_opam.Meta.depends
      [ "ocaml", {|>= "4.08.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|};
      ]
    |> add B0_opam.Meta.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"]]|}
  in
  B0_pack.v "default" ~doc:"note package" ~meta ~locked:true @@
  B0_unit.list ()
