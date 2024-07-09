open! Core
open! Async

module Start_game = struct
  module Query = struct
    type t =
      { game_kind     : Game.Game_kind.t
      ; name          : string
      ; host_and_port : Host_and_port.t
      }
    [@@deriving sexp_of, bin_io]
  end

  module Response = struct
    type t =
      | Game_started
      | Game_not_started
    [@@deriving sexp_of, bin_io]
  end

  let rpc =
    Rpc.Rpc.create
      ~name:"start-game"
      ~version:0
      ~bin_query:Query.bin_t
      ~bin_response:Response.bin_t
  ;;
end

module Take_turn = struct
  module Query = struct
    type t =
      { game     : Game.t
      ; you_play : Game.Piece.t
      }
    [@@deriving sexp_of, bin_io]
  end

  module Response = struct
    type t =
      { piece    : Game.Piece.t
      ; position : Game.Position.t
      }
    [@@deriving sexp_of, bin_io, fields]
  end

  let rpc =
    Rpc.Rpc.create
      ~name:"take-turn"
      ~version:0
      ~bin_query:Query.bin_t
      ~bin_response:Response.bin_t
  ;;
end

module Game_over = struct
  module Query = struct
    type t =
      { game       : Game.t
      ; evaluation : Game.Evaluation.t
      }
    [@@deriving sexp_of, bin_io]
  end

  module Response = struct
    type t = unit [@@deriving sexp_of, bin_io]
  end

  let rpc =
    Rpc.Rpc.create
      ~name:"game-over"
      ~version:0
      ~bin_query:Query.bin_t
      ~bin_response:Response.bin_t
  ;;
end
