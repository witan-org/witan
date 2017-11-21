
(** Command line options *)


(** {2 Type definitions} *)

type language = Input.language
(** Type of format input (taken from dolmen). *)

type input_options = {
  dir      : string;
  file     : string;
  language : language option;
}
(** The various input options. *)

type t = {

  (* Input options *)
  input : input_options;

  (* Time/Memory options *)
  time_limit  : float;
  size_limit  : float;
}
(** The aggregate type for all command line options *)


(** {2 Parsing command line} *)

val all : t Cmdliner.Term.t
(** The cdmliner term for parsing all command line options. *)

val info : Cmdliner.Term.info
(** The cmdliner info for parsing command line (includes bin name, version, etc..) *)

