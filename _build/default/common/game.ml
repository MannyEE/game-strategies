open  Core
open! Async

module Game_kind = struct
  type t =
    | Tic_tac_toe
    | Omok
  [@@deriving sexp_of, equal, bin_io]

  let to_string = Fn.compose Sexp.to_string_hum sexp_of_t

  let to_string_hum game_kind =
    game_kind
    |> sexp_of_t
    |> Sexp.to_string_hum
    |> String.lowercase
    |> String.map ~f:(function
      | '_' -> ' '
      | x   -> x)
  ;;

  let board_length = function
    | Tic_tac_toe -> 3
    | Omok        -> 15
  ;;

  let win_length = function
    | Tic_tac_toe -> 3
    | Omok        -> 5
  ;;
end

module Piece = struct
  type t =
    | X
    | O
  [@@deriving sexp, equal, compare, bin_io, enumerate]

  let of_string = Fn.compose t_of_sexp Sexp.of_string
  let to_string = Fn.compose Sexp.to_string_hum sexp_of_t

  let flip = function
    | X -> O
    | O -> X
  ;;
end

module Position = struct
  module T = struct
    type t =
      { row    : int
      ; column : int
      }
    [@@deriving sexp, equal, bin_io, compare]
  end

  include T
  include Comparable.Make_binable (T)

  let to_string = Fn.compose Sexp.to_string_hum sexp_of_t

  let in_bounds t ~game_kind =
    let board_length = Game_kind.board_length game_kind in
    let open Int.O in
    List.for_all [ t.row; t.column ] ~f:(fun x -> x >= 0 && x < board_length)
  ;;

  let down  { row; column } = { row = row + 1; column }
  let right { row; column } = { row; column = column + 1 }
  let up    { row; column } = { row = row - 1; column }
  let left  { row; column } = { row; column = column - 1 }

  let all_offsets =
    let ( >> ) = Fn.compose in
    [ up; up >> right; right; right >> down; down; down >> left; left; left >> up ]
  ;;
end

module Evaluation = struct
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

let empty game_kind = { game_kind; board = Position.Map.empty }
