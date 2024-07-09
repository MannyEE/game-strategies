open! Core
open  Game_strategies_common_lib

module Exercises : sig
  val print_game     : Game.t -> unit
  val exercise_one   : Command.t
  val exercise_two   : Command.t
  val exercise_three : Command.t
  val exercise_four  : Command.t
end

val command : Async.Command.t
