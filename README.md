Note — Declarative events and signals for OCaml
-------------------------------------------------------------------------------
%%VERSION%%

Note is an OCaml library for functional reactive programming (FRP). It
provides support to program with time varying values: declarative
events and signals.

Note also has (optional and experimental) support for reactive browser
programming with the [brr] library.

Note is distributed under the ISC license.

Homepage: <http://erratique.ch/software/note>  

[brr]: https://erratique.ch/software/brr

## Installation

Note can be installed with `opam`:

    opam install note
    opam install note brr  # For the browser support

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Documentation

The documentation can be consulted [online] or via `odig doc note`.

Questions are welcome but better asked on the [OCaml forum] than on 
the issue tracker.

[online]: http://erratique.ch/software/note/doc
[OCaml forum]: https://discuss.ocaml.org/

## Sample programs 

An implementation of the [TodoMVC] application with `note.brr` is
in [todomvc.ml](test/todomvc.ml).

You can run it with `b0 -- todomvc` see also `b0 list` for other 
tests to run.

[TodoMVC]: http://todomvc.com/

## History

Note is a *potential* successor to the OCaml [React] library.

On the plus side:

* Uses a simpler push/pull implementation which does not uses weak
  references. Combinators are easier to implement and understand. 
* Provides a formal API to interface the outputs of the reactive
  system rather than rely on effectful events and signals. Enforces
  life-time management of the output of the reactive system
  and could provides (dynamic) detection of constant signals and never
  occuring events.
* Provides (hopefully) a better combinator set. Especially with
  respect to the pain point of signal initialization in React:
  in Note, due the pull based strategy, `{E,S}.value` is safe and
  sound to use.
      
On the minus side:

* The life-time of the outputs of the reactive system have to be
  explicitely managed; but we argue this has to be done anyways in
  practice especially in browsers due to lack of weak references.
* It is easier for code interfacing the outputs of the reactive system
  to break the FRP denotational semantics and thus equational
  reasoning. However the discipline needed not to do so is clear and
  simple: do not reobserve a signal/event that was no longer observed.
* The depth first DAG update strategy of Note may be subject to
  stackoverflows on deep DAGs. We suspect however that this should not
  be a problem in practice.
  
On the unknown side:

* Memory footprint is likely to be smaller in Note. Nodes of the DAG
  do not keep track of the nodes that depend on them via weak
  references. They do keep track of the root nodes of the DAG they
  depend on, but these sets can be shared among nodes.
* Lack of weak references in Note may improve performance.
* On updates the number of nodes that have to be *visited* (not
  *recomputed*) is larger in Note. In React this is the minimal
  number of nodes *effectively* affected by the update, in Note this is
  all the nodes thay *may be* affected by the update. However
  react also needs a priority queue with weak references for its update,
  Note does not need this and allows to update the graph at any point where
  it might be needed. The latter brings API usability improvements,
  e.g. the sound and safe implementation of `{E,S}.value` in Note.
  
[React]: http://erratique.ch/software/react
