

* Regarding the
  let s1 = ...
  let s2 = ... 
  let S.swap  (E.map (fun bool -> if bool then s1 else s2))
  problem what about trying to update on reconnection ?
  This still breaks the semantics (e.g. for a state accumulating
  signal, what happened when disconnected will not be taken into
  account), but in many cases that may not matter.
  


* Does `E.fix` work ? 
* `E.S.delay_dt` in practice one needs to deal with bounds (see
  e.g. `{E,S}.until ~limit` it would be nice to get an easy treatment
  of that.

* `E.now` ??? `E.once`, `E.drop_once`.

* Try to provide support for constant cells and their observation.
  I'd be nice though if it doesn't propagate in the high-level combinators,
  smart constructors are smart but it's a bit painful to make all the
  combining cases.
  The type for cells itself could be refined or maybe only the `value`
  field of cells could be made a variant or even a boolean attribute
  (or a special `update` function that we can test to save space).


* Try to further simplify combinator implementation. In particular
  see if the init bits can be performed by [update] it's often
  almost a dupe.

* Try to lazy cells at the top level `type 'a t = ... Lazy.t` and see if
  we can provide `delay : 'a -> 'a t Lazy.t ->  'a lazy` for recursive
  definitions rather than the horrible fix point operators.

* Try to provide a story for primitive feedback.

