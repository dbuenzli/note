open B0_kit.V000

(* OCaml library names *)

let unix = B0_ocaml.libname "unix"
let brr = B0_ocaml.libname "brr"

let note = B0_ocaml.libname "note"
let note_brr = B0_ocaml.libname "note.brr"

(* Libraries *)

let note_lib =
  let srcs = [ `File ~/"src/note.mli"; `File ~/"src/note.ml" ] in
  B0_ocaml.lib note ~doc:"The note library" ~srcs

let note_brr_lib =
  let srcs = [ `Dir ~/"src/brr" ] in
  let requires = [brr; note] in
  B0_ocaml.lib note_brr ~doc:"Brr Note support" ~srcs ~requires

(* Tests *)

let test_exe ?(requires = []) src ~doc =
  let srcs = [`File src] in
  let meta = B0_meta.empty |> B0_meta.(tag test) in
  let requires = note :: requires in
  B0_ocaml.exe (Fpath.basename ~strip_ext:true src) ~srcs ~doc ~meta ~requires

let test = test_exe ~/"test/test.ml" ~doc:"Test suite"
let clock =
  test_exe ~/"test/clock.ml" ~doc:"Reactive clock example" ~requires:[unix]

let test_assets = [ `File ~/"test/base.css" ]
let test_jsoo ?(requires = [brr]) n ~doc =
  let srcs = `File ~/(Fmt.str "test/%s.ml" n) :: test_assets in
  B0_jsoo.html_page n ~requires ~srcs ~doc

let test_jsoo_module ?doc top m requires  =
  let doc = match doc with
  | None -> Fmt.str "Test %s.%s module" top m | Some doc -> doc
  in
  let test = Fmt.str "test_%s" (String.Ascii.uncapitalize m) in
  let srcs = `File (Fpath.v (Fmt.str "test/%s.ml" test)) :: test_assets in
  let meta =
    B0_meta.empty
    |> B0_meta.add B0_jsoo.compile_opts Cmd.(arg "--pretty")
  in
  B0_jsoo.html_page test ~srcs ~requires ~meta ~doc

let test_key = test_jsoo_module "Note_brr_kit" "Key" [brr; note; note_brr]
let test_mouse = test_jsoo_module "Note_brr_kit" "Mouse" [brr; note; note_brr]
let test_mutobs =
  let doc = "Test use of MutationObservers by Brr_note" in
  test_jsoo "test_mutobs" ~doc

let test_leak =
  let requires = [brr; note; note_brr] in
  test_jsoo "test_leak" ~requires ~doc:"Tests reactive DOM gc strategy"

let todomvc =
  let srcs = [ `File ~/"test/todomvc.ml"; `File ~/"test/todomvc.html" ] in
  let requires = [brr; note; note_brr;] in
  B0_jsoo.html_page "todomvc" ~requires ~srcs ~doc:"TodoMVC app"

(* Packs *)

let default =
  let meta =
    B0_meta.empty
    |> ~~ B0_meta.authors ["The note programmers"]
    |> ~~ B0_meta.maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> ~~ B0_meta.homepage "https://erratique.ch/software/note"
    |> ~~ B0_meta.online_doc "https://erratique.ch/software/note/doc/"
    |> ~~ B0_meta.licenses ["ISC"]
    |> ~~ B0_meta.repo "git+https://erratique.ch/repos/note.git"
    |> ~~ B0_meta.issues "https://github.com/dbuenzli/note/issues"
    |> ~~ B0_meta.description_tags
      ["reactive"; "declarative"; "signal"; "event"; "frp"; "org:erratique";
       "browser"; ]
    |> ~~ B0_opam.depends
      [ "ocaml", {|>= "4.08.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|};
      ]
    |> ~~ B0_opam.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"
          "--with-brr" "%{brr:installed}%"]]|}
    |> ~~ B0_opam.depopts ["brr", ""]
    |> ~~ B0_opam.conflicts [ "brr", {|< "0.0.6"|}]
    |> B0_meta.tag B0_opam.tag
  in
  B0_pack.make "default" ~doc:"note package" ~meta ~locked:true @@
  B0_unit.list ()
