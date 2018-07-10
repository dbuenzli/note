(*---------------------------------------------------------------------------
   Copyright (c) 2018 The note programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Declarative events and signals for OCaml

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1 Note} *)

(** Update steps.

    Update steps allow to schedule {e simultaneous} primitive event occurence
    and signal changes. *)
module Step : sig

  (** {1:steps Steps} *)

  type t
  (** The type for update steps. *)

  val create : unit -> t
  (** [create ()] is a new update step. *)

  val execute : t -> unit
  (** [execute step] executes the update step [step].

      @raise Invalid_argument if [step] was already executed. *)
end

(** Event and signal changes loggers.

    Loggers are the output interface of the reactive system. They
    allow external entities to observe event occurences and signal
    changes. *)
module Logr : sig

  (** {1:obs Observations} *)

  type 'a obs
  (** The type for observing changes of type ['a]. *)

  val const : 'a -> 'a obs
  (** [const v] never changes and always observes [v]. *)

  val app : ('a -> 'b) obs -> 'a obs -> 'b obs
  (** [app f v] is the observation that result from applying the
      changes of [f] to the ones of [v]. *)

  val ( $ ) : ('a -> 'b) obs -> 'a obs -> 'b obs
  (** [f $ v] is [app f v]. *)

  (** {1:loggers Loggers} *)

  type t
  (** The type for loggers. *)

  val create : ?now:bool -> unit obs -> t
  (** [create ~now o] is a logger that observes changes to [o]. If
      [now] is [true] (default) logs at least one observation before
      the call returns. *)

  val force : t -> unit
  (** [force l] observes [l] even if nothing changed. *)

  val destroy : t -> unit
  (** [destroy l] destroys log [l] this ensure that [l]'s does not
      observe any change again. The underlying observed events or
      signals also stop updating, unless they are observed by another
      logger. *)

  val hold : t -> unit
  (** [hold l] holds logger [l] to ensure it does not get garbage
      collected. *)

  val may_hold : t option -> unit
  (** [may_hold l] holds logger [Some l] to ensure it does not get
      garbage collected. Does nothing on [None]. *)

  val unhold_all : unit -> unit
  (** [unhold_all ()] {{!destroy}destroys} and unholds all loggers held
      via {!hold}. *)
end

type 'a signal
(** The type for signals of type 'a *)

type 'a event
(** The type for events of type 'a *)

(** Events.

    An event is a value with discrete occurences over time. *)
module E : sig

  (** {1:ev Events} *)

  type 'a t = 'a event
  (** The type for events with occurences of type ['a]. *)

  val obs : 'a t -> 'a option Logr.obs
  (** [obs e] is an observation for [e]'s occurences. *)

  val log : ?now:bool -> 'a event -> ('a -> unit) -> Logr.t option
  (** [log ?now e f] is [Some (Logr.(create ?now (const f $ obs e)))]
      if [e] is not {!never} and [None] otherwise. *)

  val create : unit -> 'a event * (?step:Step.t -> 'a -> unit)
  (** [create ()] is a primitive event [e] and a [send] function.
      The function [send] is such that:
      {ul
      {- [send v] generates an occurence [v] of [e] at the time it is
         called .}
      {- [send ~step v] generates an occurence [v] of [e] at the time
         [step]. The function should not be called again before [step]
         is {{!Step.execute}executed}.}}
      {b Warning.} [send] must not be used in the definition of signals
      or events. *)

  val value : 'a event -> 'a option
  (** [value e] is the value of event [e] at call time. If this is [None]
      the event has no occurence, if this is [Some v], the event occurs
      with [v]. *)

  val never : 'a event
  (** [never] is a never occuring event, \[[never]\]{_t} [= None]. *)

  val bind : 'a event -> ('a -> 'b event) -> 'b event
  (** [bind e f] is the event that results from applying [f] to
      the last event of [e]:
      {ul
      {- \[[bind e f]\]{_ t} [=] \[[f v]\]{_t} if \[[e]\]{_<=t} [= Some v].}
      {- \[[bind e f]\]{_ t} [=] [never] if \[[e]\]{_<=t} [= None].}} *)

  val join : 'a event event -> 'a event
  (** [join ee] is [bind ee (fun e -> e)]. *)

  val swap : 'a event signal -> 'a event
  (** [swap es] is the current event of [es],
      \[[swap es]\]{_t} [=] \[\[[es]\]{_t}\]{_t}. *)

  val map : ('a -> 'b) -> 'a event -> 'b event
  (** [map f e] applies [f] to [e]'s occurrences.
      {ul
      {- \[[map f e]\]{_t} [= Some (f v)] if \[[e]\]{_t} [= Some v].}
      {- \[[map f e]\]{_t} [= None] otherwise.}} *)

  val stamp : 'a -> 'b event -> 'a event
  (** [stamp v e] is [map (fun _ -> v) e] *)

  val filter : ('a -> bool) -> 'a event -> 'a event
  (** [filter p e] are the occurences of [e] that satisfy [p].
       {ul
       {- \[[filter p e]\]{_t} [= Some v] if \[[e]\]{_t} [= Some v] and
       [p v = true]}
       {- \[[filter p e]\]{_t} [= None] otherwise.}} *)

  val filter_map : ('a -> 'b option) -> 'a event -> 'b event
  (** [filter_map fm e] are [e]'s occurrences filtered and mapped by [fm].
      {ul
      {- \[[filter_map fm e]\]{_t} [= Some v] if [fm] \[[e]\]{_t} [= Some v]}
      {- \[[filter_map fm e]\]{_t} [= None] otherwise.}} *)

  val select : 'a event list -> 'a event
  (** [select el] is the occurences of every event in [el]. If more
      than one event occurs simlutanously, the leftmost is taken
      and the other are lost:
      {ul
      {- \[[select el]\]{_ t} [=] \[[List.find (fun e -> ]\[[e]\]{_t}
      [<> None) el]\]{_t}.}
      {- \[[select el]\]{_ t} [= None] otherwise.}} *)

  val accum : 'a -> ('a -> 'a) event -> 'a event
  (** [accum i e] accumulates a value, starting with [i], using [e]'s
      functional occurrences.
      {ul
      {- \[[accum i e]\]{_t} [= Some (f i)] if \[[e]\]{_t} [= Some f]
      and \[[e]\]{_<t} [= None].
      }
      {- \[[accum i e]\]{_t} [= Some (f acc)] if \[[e]\]{_t} [= Some f]
         and \[[accum i e]\]{_<t} [= Some acc].}
      {- \[[accum i e]\] [= None] otherwise.}} *)

  val fix : ('a event -> 'a event * 'b) -> 'b
  (** [fix ef] allows to refer to the value an event had an
      infinitesimal amount of time before.

      In [fix ef], [ef] is called with an event [e] that represents
      the event returned by [ef] delayed by an infinitesimal amount of
      time.  If [e', r = ef e] then [r] is returned by [fix] and [e]
      is such that :
      {ul
      {- \[[e]\]{_ t} [=] [None] if t = 0 }
      {- \[[e]\]{_ t} [=] \[[e']\]{_t-dt} otherwise}}

      {b Raises.} [Invalid_argument] if [e'] is directly a delayed event (i.e.
      an event given to a fixing function). *)

  (** {1:stdlib Stdlib types support} *)

  (** Option events *)
  module Option : sig

    val some : 'a event -> 'a option event
    (** [some e] is [map (fun v -> Some v) e]. *)

    val value : 'a option event -> 'a event
    (** [value e] is [e] whenever [e] is [Some _]:
        {ul
        {- \[[on_some e]\]{_t} [= None] if \[[e]\]{_t} [= None]}
        {- \[[on_some e]\]{_t} [= Some v] if \[[e]\]{_t} [= Some v]}} *)

    val evict : none:'a signal -> 'a option event -> 'a event
    (** [evict ~none s] is [none] when [s] is [None]:
        {ul
        {- \[[evict ~none e]\]{_t} [= None] if \[[e]\]{_t} [= None]}
        {- \[[evict ~none e]\]{_t} [= v] if \[[e]\]{_t} [= Some (Some v)]}
        {- \[[evict ~none e]\]{_t} [=] \[[none]\]{_t} if \[[s]\]{_t}
            [= Some None]}}
        [none]'s equality function is used for the resulting signal. *)
  end

  (**/**)
  val dump_src_ids : Format.formatter -> 'a event -> unit
  (**/**)
end

(** Signals.

    A signal is a value that varies continuously over time. It has
    a value at every point in time. *)
module S : sig

  (** {1:sig Signals} *)

  type 'a t = 'a signal
  (** The type for signals of type ['a]. *)

  val obs : 'a t -> 'a Logr.obs
  (** [obs s] is an observation for [s]. *)

  val log : ?now:bool -> 'a signal -> ('a -> unit) -> Logr.t
  (** [log ?now s f] is [Logr.(create ~now (const f $ obs s))]. *)

  val create :
    ?eq:('a -> 'a -> bool) -> 'a ->
    'a signal * (?step:Step.t -> 'a -> unit)
  (** [create v] is a primitive signal set to the value [v] and a
      [set] function. The function [set] is such that:
      {ul
      {- [set v] sets the signal's value to [v] at the time it is called.}
      {- [set ~step v] sets the signal value to [v] at the time it is called
         and schedules an update at time [step].}}

      {b Warning.} [set] must not be used in the definition of signals
      or events. *)

  val eq : 'a signal -> 'a -> 'a -> bool
  (** [eq s] is [s]'s equality function. *)

  val with_eq : ('a -> 'a -> bool) -> 'a signal -> 'a signal
  (** [with_eq eq s] is [s] with equality function [eq]. *)

  val value : 'a signal -> 'a
  (** [value s] is the current value of [s], \[[s]\]{_t} *)

  val rough_value : 'a signal -> 'a
  (** [rough_value s] is the current value of [s], but in contrast to
      {!value} it might not be exactly \[[s]\]{_t}. *)

  val const : ?eq:('a -> 'a -> bool) -> 'a -> 'a signal
  (** [const v] is always [v], \[[const v]\]{_t} [= v]. *)

  val hold : ?eq:('a -> 'a -> bool) -> 'a -> 'a event -> 'a signal
  (** [hold i e] has the value of [e]'s last occurrence or the
      value of [i] provides the signal value at creation time if
      there's no event at that time.
      {ul
      {- \[[hold i e]\]{_t} [= i] if \[[e]\]{_<=t} [= None]}
      {- \[[hold i e]\]{_t} [= v] if \[[e]\]{_<=t} [= Some v]}} *)

  val bind : 'a signal -> ('a -> 'b signal) -> 'b signal
  (** [bind s f] is the signal that results from applying [f] to
      [s], \[[bind s f]\]{_ t} [=] \[f\[[s]\]{_t}\]{_t}. *)

  val join : 'a signal signal -> 'a signal
  (** [join ss] is [bind ss (fun s -> s)]. *)

  val swap : 'a signal -> 'a signal event -> 'a signal
  (** [swap s se] is [join (hold ~eq:( == ) s se)] that is the values of
      [s] followed by values of the last signal that occured on [se]. *)

  val changes : 'a signal -> 'a event
  (** [changes s] occurs with the value of [s] whenever it changes.
      {ul
      {- \[[changes s]\]{_t} [= Some v]
      if \[[s]\]{_t} [= v] and \[[s]\]{_t-dt} [= v'] and [eq v v' = false].}
      {- \[[changes s]\]{_t} [= None] otherwise.}}

      {b Warning.} By definition no event occurs if [s] changes at
      creation time ([0 - dt] is undefined). *)

  val map : ?eq:('b -> 'b -> bool) -> ('a -> 'b) -> 'a signal -> 'b signal
  (** [map f s] is [s] transformed by [f],
      \[[map f s]\]{_t} = [f] \[[s]\]{_t}. *)

  val app :
    ?eq:('b -> 'b -> bool) -> ('a -> 'b) signal -> 'a signal -> 'b signal
  (** [app sf s] holds the value of [sf] applied to the value of [s],
      \[[app sf s]\]{_t} [=] \[[sf]\]{_t} \[[s]\]{_t}. *)

  val delay : 'a -> 'a signal Lazy.t -> 'a signal
  (** [delay i (lazy s)] is the value [s] had an infinitesimal amount
      of time before:
      {ul
      {- \[[delay i (lazy s)]\]{_ t} [=] [i] for t = 0. }
      {- \[[delay i (lazy s)]\]{_ t} [=] \[[s']\]{_t-dt} otherwise.}} *)

  val accum : ?eq:('a -> 'a -> bool) -> 'a -> ('a -> 'a) event -> 'a signal
  (** [accum i e] is [hold i (E.accum i e)]. *)

  val fix : 'a -> ('a signal -> 'a signal * 'b) -> 'b
  (** In [fix sf], [sf] is called with a signal [s] that represents

      the signal returned by [sf] delayed by an infinitesimal amount
      time. If [s', r = sf s] then [r] is returned by [fix] and [s]
      is such that :
      {ul
      {- \[[s]\]{_ t} [=] [i] for t = 0. }
      {- \[[s]\]{_ t} [=] \[[s']\]{_t-dt} otherwise.}} *)

(*
  val active : on:bool signal -> 'a signal -> 'a signal
  (** [active ~on s] is has the value of [s] at creation
      time and then mirrors [s] whenever [on] is [true].
      When [on] is false holds the last value [s] had when [on]
      was true.
      {ul
      {- \[[active ~on s]\]{_0} [=] \[[s]\]{_0}}
      {- \[[active ~on s]\]{_t} [=] \[[s]\]{_t} if \[[on]\]{_t} [= true]}
      {- \[[active ~on s]\]{_t} [=] \[[s]\]{_t'} if \[[on]\]{_t} [= false]
         where t' is the greatest 0 < t' < t with \[[on]\]{_t'} [= true].}} *)
*)

 (** {1:lifting Lifting}
     Lifting combinators. For a given [n] the semantics is :
     \[[ln f a1] ... [an]\]{_t} = f \[[a1]\]{_t} ... \[[an]\]{_t} *)

  val l1 : ?eq:('b -> 'b -> bool) -> ('a -> 'b) -> 'a signal -> 'b signal
  val l2 :
    ?eq:('c -> 'c -> bool) -> ('a -> 'b -> 'c) -> 'a signal -> 'b signal ->
    'c signal

  (** {1:stdlib Stdlib types support} *)

  (** Boolean signals *)
  module Bool : sig

    val no : bool signal
    (** [no] is [const false]. *)

    val yes : bool signal
    (** [yes] is [const true]. *)

    val not : bool signal -> bool signal
    (** [not s] is [map not s]. *)

    val ( && ) : bool signal -> bool signal -> bool signal
    (** [s0 && s1] is [l2 ( && ) s1 s2]. *)

    val ( || ) : bool signal -> bool signal -> bool signal
    (** [s0 || s1] is [l2 ( || ) s1 s2]. *)

    val edge : bool signal -> bool event
    (** [edge s] is [changes s]. *)

    val rise : bool signal -> unit event
    (** [rise s] is
        [E.filter_map (function true -> Some b | false -> None) (edge s)]. *)

    val fall : bool signal -> unit event
   (** [fall s] is
       [E.filter_map (function true -> None | None -> Some b) (edge s)] *)

    val flip : init:bool -> 'a event -> bool signal
    (** [flip ~init e] is a signal whose boolean value flips each time
        [e] occurs. [init] provides the signal value at creation time.
          {ul
          {- \[[flip b e]\]{_0} [= not b] if \[[e]\]{_0} [= Some _]}
          {- \[[flip b e]\]{_t} [=] init if \[[e]\]{_<=t} [= None]}
          {- \[[flip b e]\]{_t} [=] [not] \[[flip b e]\]{_t-dt}
             if \[[e]\]{_t} [= Some _]}} *)
  end

  (** Option signals *)
  module Option : sig

    val eq : ('a -> 'a -> bool) -> ('a option -> 'a option -> bool)
    (** [eq f] derives an equality function on options using [f] for
        testing [Some _]. *)

    val none : 'a option signal
    (** [none] is [const None]. *)

    val some : 'a signal -> 'a option signal
    (** [some s] is [map (fun v -> Some v) s] and uses [s]'s equality
        function to derive the obvious one on options. *)

    val hold_value : 'a -> 'a option signal -> 'a signal
    (** [hold_value i s] is the last [Some _] value of [s] or
        [i] if there was no such value:
        {ul
        {- \[[hold_some i s]\]{_t} [= i] if \[[s]\]{_<t} [= None]}
        {- \[[hold_some i s]\]{_t} [= v] if \[[s]\]{_<=t} [= Some v]}}
        Uses [s]'s equality on [Some _]. *)

    val evict : none:'a signal -> 'a option signal -> 'a signal
    (** [evict ~none s] is [none] when [s] is [None]:
        {ul
        {- \[[evict ~none s]\]{_t} [= v] if \[[s]\]{_t} [= Some v]}
        {- \[[evict ~none s]\]{_t} [=] \[[none]\]{_t} if \[[s]\]{_t} [= None]}}
        [none]'s equality function is used for the resulting signal. *)
  end

  (**/**)
  val dump_src_ids : Format.formatter -> 'a signal -> unit
  (**/**)
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
