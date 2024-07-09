open! Core
open! Async
open! Game_strategies_common_lib

module Exercises = struct
  (* Here are some functions which know how to create a couple different
     kinds of games *)
  let empty_game = Game.empty Game.Game_kind.Tic_tac_toe

  let place_piece (game : Game.t) ~piece ~position : Game.t =
    let board = Map.set game.board ~key:position ~data:piece in
    { game with board }
  ;;

  let win_for_x =
    let open Game in
    empty_game
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 1 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 1 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 1 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 2 }
  ;;

  let non_win =
    let open Game in
    empty_game
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
  ;;

  let print_game (game : Game.t) =
    let game_len = Game.Game_kind.board_length game.game_kind in
    let _row_list =
      List.init game_len ~f:(fun row ->
        let _col_list =
          List.init game_len ~f:(fun col ->
            let cur_position =
              Map.find
                game.board
                { Game.Position.row = game_len - row - 1
                ; column = game_len - col - 1
                }
            in
            (match cur_position with
             | None -> print_string " "
             | Some position ->
               (match position with
                | X -> print_string "X"
                | O -> print_string "O"));
            match col with
            | 0 -> print_newline ()
            | _ -> print_string " | ")
        in

        match row with 
        | 0 -> ()
        | _ -> print_endline "---------")
    in
    ()
  ;;

  let%expect_test "print_win_for_x" =
    print_game win_for_x;
    [%expect
      {|
      X | O | X
      ---------
      O | O | X
      ---------
      O | X | X
      |}];
    return ()
  ;;

  let%expect_test "print_non_win" =
    print_game non_win;
    [%expect
      {|
      X |   |
      ---------
      O |   |
      ---------
      O |   | X
      |}];
    return ()
  ;;

  (* Exercise 1 *)
  let available_moves (game : Game.t) : Game.Position.t list =

    let game_len = Game.Game_kind.board_length game.game_kind in
    List.init (game_len * game_len) ~f:(fun pos ->

      let row = (pos) / 3 in
      let col = (pos) % 3 in

      let cur_position = Map.find game.board { Game.Position.row = row; column = col} in
      match cur_position with
        | None ->  [ { Game.Position.row = row; column = col}]
        | Some _ -> [] )
    |> List.concat
    
  ;;

  let%expect_test "available_spaces" =
    let cur_available_moves = available_moves non_win in
    List.iter cur_available_moves ~f:(fun position -> 
      print_endline (Game.Position.to_string position));

    [%expect
      {|
      ((row 0) (column 1))
      ((row 0) (column 2))
      ((row 1) (column 1))
      ((row 1) (column 2))
      ((row 2) (column 1))
      |}];
    return ()
;;

  (* Exercise 2 *)
  let evaluate (game : Game.t) : Game.Evaluation.t =
    ignore game;
    failwith "Implement me!"
  ;;

  (* Exercise 3 *)
  let winning_moves ~(me : Game.Piece.t) (game : Game.t)
    : Game.Position.t list
    =
    ignore me;
    ignore game;
    failwith "Implement me!"
  ;;

  (* Exercise 4 *)
  let losing_moves ~(me : Game.Piece.t) (game : Game.t)
    : Game.Position.t list
    =
    ignore me;
    ignore game;
    failwith "Implement me!"
  ;;

  let exercise_one =
    Command.async
      ~summary:"Exercise 1: Where can I move?"
      (let%map_open.Command () = return () in
       fun () ->
         let moves = available_moves win_for_x in
         print_s [%sexp (moves : Game.Position.t list)];
         let moves = available_moves non_win in
         print_s [%sexp (moves : Game.Position.t list)];
         return ())
  ;;

  let exercise_two =
    Command.async
      ~summary:"Exercise 2: Is the game over?"
      (let%map_open.Command () = return () in
       fun () ->
         let evaluation = evaluate win_for_x in
         print_s [%sexp (evaluation : Game.Evaluation.t)];
         let evaluation = evaluate win_for_x in
         print_s [%sexp (evaluation : Game.Evaluation.t)];
         return ())
  ;;

  let piece_flag =
    let open Command.Param in
    flag
      "piece"
      (required (Arg_type.create Game.Piece.of_string))
      ~doc:
        ("PIECE "
         ^ (Game.Piece.all
            |> List.map ~f:Game.Piece.to_string
            |> String.concat ~sep:", "))
  ;;

  let exercise_three =
    Command.async
      ~summary:"Exercise 3: Is there a winning move?"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         let winning_moves = winning_moves ~me:piece non_win in
         print_s [%sexp (winning_moves : Game.Position.t list)];
         return ())
  ;;

  let exercise_four =
    Command.async
      ~summary:"Exercise 4: Is there a losing move?"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         let losing_moves = losing_moves ~me:piece non_win in
         print_s [%sexp (losing_moves : Game.Position.t list)];
         return ())
  ;;

  let command =
    Command.group
      ~summary:"Exercises"
      [ "one", exercise_one
      ; "two", exercise_two
      ; "three", exercise_three
      ; "four", exercise_four
      ]
  ;;
end

let handle_turn (_client : unit) _query =
  let next_position = Game.Position.{ row = 0; column = 0 } in
  let cur_piece = Game.Piece.X in
  let response =
    Rpcs.Take_turn.Response.{ piece = cur_piece; position = next_position }
  in
  return response
;;

let implementations =
  Rpc.Implementations.create_exn
    ~on_unknown_rpc:`Close_connection
    ~implementations:[ Rpc.Rpc.implement Rpcs.Take_turn.rpc handle_turn ]
;;

let command_play =
  Command.async
    ~summary:"Play"
    (let%map_open.Command () = return ()
     (* and _controller = flag "-controller" (required host_and_port) ~doc:"_
        host_and_port of controller"*)
     and port = flag "-port" (required int) ~doc:"_ port to listen on" in
     fun () ->
       (* We should start listing on the supplied [port], ready to handle
          incoming queries for [Take_turn] and [Game_over]. We should also
          connect to the controller and send a [Start_game] to initiate the
          game. *)
       let%bind server =
         Rpc.Connection.serve
           ~implementations
           ~initial_connection_state:(fun _client_identity _client_addr ->
             ())
           ~where_to_listen:(Tcp.Where_to_listen.of_port port)
           ()
       in
       (* ignore controller; *)
       ignore port;
       Tcp.Server.close_finished server)
;;

let command =
  Command.group
    ~summary:"Game Strategies"
    [ "play", command_play; "exercises", Exercises.command ]
;;
