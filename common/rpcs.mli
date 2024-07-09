open! Core
open! Async

module Start_game : sig
  module Query : sig
    type t =
      { game_kind     : Game.Game_kind.t
      ; name          : string
      ; host_and_port : Host_and_port.t
      }
    [@@deriving sexp_of, bin_io]
  end

  module Response : sig
    type t =
      | Game_started
      | Game_not_started
    [@@deriving sexp_of, bin_io]
  end

  val rpc : (Query.t, Response.t) Rpc.Rpc.t
end

module Take_turn : sig
  module Query : sig
    type t =
      { game     : Game.t
      ; you_play : Game.Piece.t
      }
    [@@deriving sexp_of, bin_io]
  end

  module Response : sig
    type t =
      { piece    : Game.Piece.t
      ; position : Game.Position.t
      }
    [@@deriving sexp_of, bin_io, fields]
  end

  val rpc : (Query.t, Response.t) Rpc.Rpc.t
end

module Game_over : sig
  module Query : sig
    type t =
      { game       : Game.t
      ; evaluation : Game.Evaluation.t
      }
    [@@deriving sexp_of, bin_io]
  end

  module Response : sig
    type t = unit [@@deriving sexp_of, bin_io]
  end

  val rpc : (Query.t, Response.t) Rpc.Rpc.t
end
