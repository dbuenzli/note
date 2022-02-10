(*---------------------------------------------------------------------------
   Copyright (c) 2018 The note programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Declarative events and signals for OCaml. *)


(** {1 Note} *)

(** Update steps.

    Update steps allow to schedule {e simultaneous} primitive event occurrence
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
    allow external entities to observe event occurrences and signal
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

    An event is a value with discrete occurrences over time.
    Consult the {{!page-semantics.events}semantics and notations} of
    events. *)
module E : sig

  (** {1:ev Events} *)

  type 'a t = 'a event
  (** The type for events with occurrences of type ['a]. *)

  type 'a send = ?step:Step.t -> 'a -> unit
  (** The type for functions sending event occurrences of type ['a].
      See {!create}. *)

  val obs : 'a t -> 'a option Logr.obs
  (** [obs e] is an observation for [e]'s occurrences. *)

  val log : ?now:bool -> 'a event -> ('a -> unit) -> Logr.t option
  (** [log ?now e f] is [Some (Logr.(create ?now (const f $ obs e)))]
      if [e] is not {!never} and [None] otherwise. *)

  val create : unit -> 'a event * 'a send
  (** [create ()] is a primitive event [e] and a [send] function.
      The function [send] is such that:
      {ul
      {- [send v] generates an occurrence [v] of [e] at the time it is
         called .}
      {- [send ~step v] generates an occurrence [v] of [e] at the time
         [step]. The function should not be called again before [step]
         is {{!Step.execute}executed}.}}
      {b Warning.} [send] must not be used in the definition of signals
      or events. *)

  val value : 'a event -> 'a option
  (** [value e] is the value of event [e] at call time. If this is [None]
      the event has no occurrence, if this is [Some v], the event occurs
      with [v]. *)

  val never : 'a event
  (** [never] is a never occuring event, \[[never]\]{_t} [= None]. *)

  val bind : 'a event -> ('a -> 'b event) -> 'b event
  (** [bind e f] is the event that results from applying [f] to
      the last event of [e]:
      {ul
      {- \[[bind e f]\]{_ t} [=] \[[f v]\]{_t} if \[[e]\]{_≤t} [= Some v].}
      {- \[[bind e f]\]{_ t} [=] [never]{_t} if \[[e]\]{_≤t} [= None].}} *)

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

  val stamp : 'b event -> 'a -> 'a event
  (** [stamp e v] is [map e (fun _ -> v)] *)

  val filter : ('a -> bool) -> 'a event -> 'a event
  (** [filter p e] are the occurrences of [e] that satisfy [p].
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
  (** [select el] is the occurrences of every event in [el]. If more
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

  val until : ?limit:bool -> next:'a event -> 'b event -> 'b event
  (** [until ~limit ~next e] is [e]'s occurrences until [next]
      occurs. At that point if [e] occurs simultaneously the occurrence is
      discarded ([limit] is [false], default) or kept ([limit] is [true])
      and after this the event never occurs again.
      {ul
      {- \[[until ~limit ~next e]\]{_t} [=] \[[e]\]{_t}
         if \[[next]\]{_≤t} [= None]}
      {- \[[until ~limit:false ~next e]\]{_t} [= None]
         if \[[next]\]{_t} [= Some _] and \[[next]\]{_<t} [= None].}
      {- \[[until ~limit:true ~next e]\]{_t} [=] \[[e]\]{_t}
         if \[[next]\]{_t} [= Some _] and \[[next]\]{_<t} [= None].}
      {- \[[until ~limit ~next e]\]{_t} [= None] otherwise.}} *)

  val follow : 'a event -> on:bool signal -> 'a event
  (** [follow e ~on] is [e]'s occurrences whenever [on] is [true].
      {ul
      {- \[[follow e ~on]\]{_t} [=] \[[e]\]{_t} if \[[on]\]{_t} [= true]}
      {- \[[follow e ~on]\]{_t} [= None]  if \[[on]\]{_t} [= false]}} *)

  val defer : 'a event -> 'a event
  (** [defer s] is [s] delayed by an infinitesimal amount of time.
      At creation time [init] is used (defaults to [S.value s]).
      {ul
      {- \[[defer e]\]{_ t} [=] [None] for t = 0. }
      {- \[[defer e]\]{_ t} [=] \[[e]\]{_t-dt} otherwise.}} *)

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

    val on_some : 'a option event -> 'a event
    (** [on_some e] is [e] when [Some _] occurs:
        {ul
        {- \[[on_some e]\]{_t} [= Some v] if \[[e]\]{_t} [= Some (Some v)]}
        {- \[[on_some e]\]{_t} [= None] otherwise.}} *)

    (** {1:lift Lifted {!Stdlib.Option} module} *)

    val some : 'a event -> 'a option event
    (** [some e] is [map (fun v -> Some v) e]. *)

    val value : 'a option event -> default:'a signal -> 'a event
    (** [value e default] is [default] when [e] occurs with [None]:
        {ul
        {- \[[value e ~default]\]{_t} [= None] if \[[e]\]{_t} [= None]}
        {- \[[value e ~default]\]{_t} [= Some v]
           if \[[e]\]{_t} [= Some (Some v)]}
        {- \[[value e ~default]\]{_t} [=] \[[default]\]{_t} if \[[e]\]{_t}
            [= Some None]}} *)

    val get : 'a option event -> 'a event
    (** [get e] is [map get e]. *)

    val bind : 'a option event -> ('a -> 'b option) -> 'b option event
    (** [bind e f] is [map (fun v -> Option.bind v f) e]. *)

    val join : 'a option option event -> 'a option event
    (** [join e] is [map Option.join e]. *)

    val map : ('a -> 'b) -> 'a option event -> 'b option event
    (** [map f e] is [map Option.map e]. *)

    val is_none : 'a option event -> bool event
    (** [is_none e] is [map is_none e]. *)

    val is_some : 'a option event -> bool event
    (** [is_some e] is [map is_some e]. *)
  end

  (** Pair events. *)
  module Pair : sig
    val fst : ('a * 'b) event -> 'a event
    (** [fst e] is [map fst e]. *)

    val snd : ('a * 'b) event -> 'b event
    (** [snd e] is [map snd e]. *)

    val v : 'a event -> 'b event -> ('a * 'b) event
    (** [v e0 e1] pair simultaneous occurrences of [e0] and [e1]:
        {ul
        {- \[[v e0 e1]\]{_t} [= Some (v0, v1)] if \[[e0]\]{_t} [= Some v0]
           and \[[e1]\]{_t} [= Some v1]}
        {- \[[v e0 e1]\]{_t} [= None] otherwise.}} *)
  end

  (**/**)
  val dump_src_ids : Format.formatter -> 'a event -> unit
  (**/**)
end

(** Signals.

    A signal is a value that varies continuously over time. Consult
    the {!page-semantics.signals}semantics and notations} of signals. *)
module S : sig

  (** {1:sig Signals} *)

  type 'a t = 'a signal
  (** The type for signals of type ['a]. *)

  type 'a set = ?step:Step.t -> 'a -> unit
  (** The type for functions setting signal values of type ['a].
      See {!create}.*)

  val obs : 'a t -> 'a Logr.obs
  (** [obs s] is an observation for [s]. *)

  val log : ?now:bool -> 'a signal -> ('a -> unit) -> Logr.t
  (** [log ?now s f] is [Logr.(create ~now (const f $ obs s))]. *)

  val create : ?eq:('a -> 'a -> bool) -> 'a -> 'a signal * 'a set
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
      {- \[[hold i e]\]{_t} [= i] if \[[e]\]{_≤t} [= None]}
      {- \[[hold i e]\]{_t} [= v] if \[[e]\]{_≤t} [= Some v]}} *)

  val bind : 'a signal -> ('a -> 'b signal) -> 'b signal
  (** [bind s f] is the signal that results from applying [f] to
      [s], \[[bind s f]\]{_ t} [=] \[f\[[s]\]{_t}\]{_t}. *)

  val join : 'a signal signal -> 'a signal
  (** [join ss] is [bind ss (fun s -> s)]. *)

  val swap : 'a signal -> 'a signal event -> 'a signal
  (** [swap s se] is [join (hold ~eq:( == ) s se)] that is the values of
      [s] followed by values of the last signal that occurred on [se]. *)

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

  val sample : 'b signal -> on:'a event -> ('b -> 'a -> 'c) -> 'c event
  (** [sample s ~on f] samples [s] at [e]'s occurrences.
      {ul
      {- \[[sample s ~on f]\]{_t} [= Some (f sv ev)] if \[[on]\]{_t} [= Some ev]
         and  \[[s]\]{_t} [= sv].}
      {- \[[sample s ~on f]\]{_t} [= None] otherwise.}} *)

  val sample_filter :
    'b signal -> on:'a event -> ('b -> 'a -> 'c option) -> 'c event
  (** [sample_filter s on f] is [E.Option.on_some (sample s ~on f)]. *)

  val snapshot : 'b signal -> on:'a event -> 'b event
  (** [snapshot ~on s] is [sample (fun v _ -> v) ~on s].

      {b TODO.} Candidate for deletion. *)

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

  val accum : ?eq:('a -> 'a -> bool) -> 'a -> ('a -> 'a) event -> 'a signal
  (** [accum i e] is [hold i (E.accum i e)]. *)

  val until : ?limit:bool -> ?init:'b -> next:'a event -> 'b signal -> 'b signal
  (** [until ~limit ~init ~next s] is [s] until [next] occurs, after
      which the value [s] had just before ([limit] is [false], default)
      or whenever [next] occurs ([limit] is [true]) is kept forever.
      {ul
      {- \[[until ~limit ~init ~next s]\]{_t} [=] \[[s]\]{_t}
         if \[[next]\]{_≤t} [= None]}
      {- \[[until ~limit ~init ~next s]\]{_t}
         [= init] if \[[next]\]{_0} [= Some _]}
      {- \[[until ~limit:false ~init ~next s]\]{_t} [=] \[[s]\]{_t'- dt}
         if \[[next]\]{_t'} [= Some _] and \[[next]\]{_<t'} [= None].}
      {- \[[until ~limit:true ~init ~next s]\]{_t} [=] \[[s]\]{_t'}
         if \[[next]\]{_t'} [= Some _] and \[[next]\]{_<t'} [= None].}}
      [init] defaults to [value s]. *)

  val follow : ?init:'a -> 'a signal -> on:bool signal -> 'a signal
  (** [follow ~init s ~on] is [s] whenever [on] is [true] and the last
      value of [s] when [on] was [true] if [on] is [false]. If [on] is
      [false] at creation time [init] is used (defaults to [S.value
      s]).
      {ul
      {- \[[follow ~init s ~on]\]{_0} [=] \[[s]\]{_0}
         if \[[on]\]{_0} [= true]}
      {- \[[follow ~init s ~on]\]{_0} [=] \[[init]\]{_0}
         if \[[on]\]{_0} [= false]}
      {- \[[follow ~init s ~on]\]{_t} [=] \[[s]\]{_t}
         if \[[on]\]{_t} [= true]}
      {- \[[follow ~init s ~on]\]{_t} [=] \[[follow ~init s ~on]\]{_t'}
         if \[[on]\]{_t} [= false] where t' is the
         greatest t' < t with \[[on]\]{_t'} [= true] or [0] if there
         is no such time.}} *)

  val defer : ?init:'a -> 'a signal -> 'a signal
  (** [defer s] is [s] delayed by an infinitesimal amount of time.
      At creation time [init] is used (defaults to [S.value s]).
      {ul
      {- \[[defer s]\]{_ t} [=] [init] for t = 0. }
      {- \[[defer s]\]{_ t} [=] \[[s]\]{_t-dt} otherwise.}} *)

  val delay : 'a -> 'a signal Lazy.t -> 'a signal
  (** [delay i (lazy s)] is the value [s] had an infinitesimal amount
      of time before:
      {ul
      {- \[[delay i (lazy s)]\]{_ t} [=] [i] for t = 0. }
      {- \[[delay i (lazy s)]\]{_ t} [=] \[[s']\]{_t-dt} otherwise.}} *)

  val fix : ?eq:('a -> 'a -> bool) -> 'a -> ('a signal -> 'a signal * 'b) -> 'b
  (** In [fix sf], [sf] is called with a signal [s] that represents

      the signal returned by [sf] delayed by an infinitesimal amount
      time. If [s', r = sf s] then [r] is returned by [fix] and [s]
      is such that :
      {ul
      {- \[[s]\]{_ t} [=] [i] for t = 0. }
      {- \[[s]\]{_ t} [=] \[[s']\]{_t-dt} otherwise.}} *)

 (** {1:lifting Lifting}
     Lifting combinators. For a given [n] the semantics is :
     \[[ln f a1] ... [an]\]{_t} = f \[[a1]\]{_t} ... \[[an]\]{_t} *)

  val l1 : ?eq:('b -> 'b -> bool) -> ('a -> 'b) -> 'a signal -> 'b signal
  val l2 :
    ?eq:('c -> 'c -> bool) -> ('a -> 'b -> 'c) -> 'a signal -> 'b signal ->
    'c signal
  val l3 :
    ?eq:('d -> 'd -> bool) -> ('a -> 'b -> 'c -> 'd) -> 'a signal ->
    'b signal -> 'c signal -> 'd signal

  (** {1:stdlib Stdlib types support} *)

  (** Boolean signals *)
  module Bool : sig

    val false' : bool signal
    (** [false'] is [const false]. *)

    val true' : bool signal
    (** [true'] is [const true]. *)

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
          {- \[[flip b e]\]{_t} [=] init if \[[e]\]{_≤t} [= None]}
          {- \[[flip b e]\]{_t} [=] [not] \[[flip b e]\]{_t-dt}
             if \[[e]\]{_t} [= Some _]}} *)
  end

  (** Option signals *)
  module Option : sig

    val eq : ('a -> 'a -> bool) -> ('a option -> 'a option -> bool)
    (** [eq f] derives an equality function on options using [f] for
        testing [Some _]. *)

    val hold_value : 'a -> 'a option signal -> 'a signal
    (** [hold_value i s] is the last [Some _] value of [s] or
        [i] if there was no such value:
        {ul
        {- \[[hold_some i s]\]{_t} [= i] if \[[s]\]{_<t} [= None]}
        {- \[[hold_some i s]\]{_t} [= v] if \[[s]\]{_≤t} [= Some v]}}
        Uses [s]'s equality on [Some _]. *)

    (** {1:lift Lifted {!Stdlib.Option} module} *)

    val none : 'a option signal
    (** [none] is [const None]. *)

    val some : 'a signal -> 'a option signal
    (** [some s] is [map (fun v -> Some v) s] and uses [s]'s equality
        function to derive the obvious one on options. *)

    val value : 'a option signal -> default:'a signal -> 'a signal
    (** [value s ~default] is [default] when [s] is [None]:
        {ul
        {- \[[value s ~default]\]{_t} [= v] if \[[s]\]{_t} [= Some v]}
        {- \[[value s ~default]\]{_t} [=]
           \[[default]\]{_t} if \[[s]\]{_t} [= None]}}
        [default]'s equality function is used for the resulting signal. *)

    val get : ?eq:('a -> 'a -> bool) -> 'a option signal -> 'a signal
    (** [get s] is [map ~eq Option.get s]. *)

    val bind :
      ?eq:('b option -> 'b option -> bool) -> 'a option signal ->
      ('a -> 'b option) -> 'b option signal
    (** [bind ~eq s f] is [map ~eq (fun v -> Option.bind v f) s]. *)

    val join :
      ?eq:('a option -> 'a option -> bool) -> 'a option option signal ->
      'a option signal
    (** [join ~eq oos] is [map ~eq Option.join oos]. *)

    val map :
      ?eq:('b option -> 'b option -> bool) -> ('a -> 'b) ->
      'a option signal -> 'b option signal
    (** [map ~eq f s] is [map ~eq Option.map s]. *)

    val is_none : 'a option signal -> bool signal
    (** [is_none s] is [map Option.is_none s]. *)

    val is_some : 'a option signal -> bool signal
    (** [is_some s] is [map Option.is_some s]. *)
  end

  (** Pair signals. *)
  module Pair : sig

    val fst : ?eq:('a -> 'a -> bool) -> ('a * 'b) signal -> 'a signal
    (** [fst ?eq s] is [map ?eq fst s]. *)

    val snd : ?eq:('b -> 'b -> bool) -> ('a * 'b) signal -> 'b signal
    (** [snd ?eq e] is [map ?eq snd e]. *)

    val v : 'a signal -> 'b signal -> ('a * 'b) signal
    (** [v s0 s1] is [l2 (fun x y -> (x, y) s0 s1]. *)
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
