open! Core

module Game_kind : sig
  type t =
    | Tic_tac_toe
    | Omok
  [@@deriving sexp_of, equal, bin_io]

  val to_string     : t -> string
  val to_string_hum : t -> string

  (** [board_length] returns the length of the board. 3 for [ Tic_tac_toe ]
    and 15 for [Omok]. *)
  val board_length  : t -> int

  (** [win_length] returns the winning length of the board. 3 for
    [ Tic_tac_toe ] and 5 for [Omok]. *)
  val win_length    : t -> int
end

module Piece : sig
  type t =
    | X
    | O
  [@@deriving sexp_of, equal, bin_io, enumerate]

  val of_string : string -> t
  val to_string : t -> string

  (* [flip] gives you the "other" piece. | X -> O | O -> X *)
  val flip : t -> t
end

module Position : sig
  (* Top-left is [{row = 0; column = 0}].

     row indexes increment downwards.

     column indexes increment rightwards. *)
  type t =
    { row    : int
    ; column : int
    }
  [@@deriving sexp_of, equal, bin_io, compare]

  val to_string : t -> string
  val in_bounds : t -> game_kind:Game_kind.t -> bool

  (** [down t] is [t]'s downwards neighbor. *)
  val down      : t -> t

  (** [right t] is [t]'s rightwards neighbor. *)
  val right     : t -> t

  (** [up t] is [t]'s upwards neighbor. *)
  val up        : t -> t

  (** [left t] is [t]'s leftwards neighbor. *)
  val left      : t -> t

  (** [all_offsets] is a list of functions to compute all 8 neighbors of a
    cell (i.e. left, up-left, up, up-right, right, right-down, down,
    down-left). *)
  val all_offsets : (t -> t) list

  include Comparable.S_plain with type t := t
end

module Evaluation : sig
  type t =
    | Illegal_move
    | Game_continues
    | Game_over of { winner : Piece.t option }
  [@@deriving sexp_of, bin_io]
end

type t =
  { game_kind : Game_kind.t
  ; board     : Piece.t Position.Map.t
  }
[@@deriving sexp_of, bin_io]

val empty : Game_kind.t -> t
