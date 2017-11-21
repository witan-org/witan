
(** Problem input for Witan *)

(** {2 Parsing some input} *)

type language =
  | Dimacs  (** The dimacs language *)
  | ICNF    (** iCNF is ane xtension of dimacs *)
  | Smtlib  (** smtlib language *)
  | Tptp    (** TPTP problems language *)
  | Zf      (** Zipperposition format *)
(** The type of input language supported. *)

val enum : (string * language) list
(** Enumeration of pairs of a language and its name, mainly for use by cmdliner. *)

val read : ?language:language -> dir:string -> string -> Dolmen.Statement.t Gen.t
(** Read a file in a directory. Automatically expands all include statements.
    @language: if set, overrides input language auto-detection performed by dolmen. *)
