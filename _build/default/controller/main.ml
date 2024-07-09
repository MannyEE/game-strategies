open! Core
open! Async
open Game_strategies_common_lib

let start_game_impl _client (query : Rpcs.Start_game.Query.t) =
  print_s [%message "Query received" (query : Rpcs.Start_game.Query.t)];
  let game_over_query =
    { Rpcs.Game_over.Query.game = Game.empty Game.Game_kind.Tic_tac_toe
    ; evaluation = Game.Evaluation.Game_over { winner = None }
    }
  in
  let%bind start_game_response =
    Rpc.Connection.with_client
      (Tcp.Where_to_connect.of_host_and_port query.host_and_port)
      (fun conn -> Rpc.Rpc.dispatch_exn Rpcs.Game_over.rpc conn game_over_query)
  in
  print_s
    [%message
      "start_game_response"
        (start_game_response : (Rpcs.Game_over.Response.t, exn) Result.t)];
  return Rpcs.Start_game.Response.Game_not_started
;;

let implementations =
  Rpc.Implementations.create_exn
    ~on_unknown_rpc:`Close_connection
    ~implementations:[ Rpc.Rpc.implement Rpcs.Start_game.rpc start_game_impl ]
;;

let command =
  Command.async
    ~summary:"Client"
    (let%map_open.Command () = return ()
     and port = flag "-port" (required int) ~doc:"INT server port" in
     fun () ->
       let%bind server =
         Rpc.Connection.serve
           ~implementations
           ~initial_connection_state:(fun _client_identity _client_addr -> ())
           ~where_to_listen:(Tcp.Where_to_listen.of_port port)
           ()
       in
       Tcp.Server.close_finished server)
;;
