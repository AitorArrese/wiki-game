open! Core

module Direction = struct
  type t =
    | Left
    | Up
    | Right
    | Down
    | ()
end

module Coordinate = struct
  module T = struct
    type t =
      { row : int
      ; col : int
      }
    [@@deriving compare, sexp, hash, equal]
  end

  include Comparable.Make (T)
  include T
end

type game =
  { mutable player : Coordinate.t
  ; mutable goal : Coordinate.t
  ; open_spaces : Coordinate.t Hash_set.t
  }

let create_game (content : string list) =
  let open_spaces = Hash_set.create (module Coordinate) in
  let this_game =
    { player = { row = 0; col = 0 }
    ; goal = { row = 0; col = 0 }
    ; open_spaces
    }
  in
  List.iteri content ~f:(fun row str ->
    String.iteri str ~f:(fun col char ->
      match char with
      | '.' -> Hash_set.add open_spaces { row; col }
      | 'S' ->
        Hash_set.add open_spaces { row; col };
        this_game.player <- { row; col }
      | 'E' ->
        Hash_set.add open_spaces { row; col };
        this_game.goal <- { row; col }
      | _ -> ()));
  this_game
;;

let find_neighbors (cord : Coordinate.t) game =
  let possible_moves =
    [ { cord with col = cord.col + 1 }, Direction.Right
    ; { cord with row = cord.row + 1 }, Direction.Down
    ; { cord with col = cord.col - 1 }, Direction.Left
    ; { cord with row = cord.row - 1 }, Direction.Up
    ]
  in
  List.filter possible_moves ~f:(fun (cord, _dir) ->
    Hash_set.mem game.open_spaces cord)
;;

let find_path game =
  let visited = Hash_set.create (module Coordinate) in
  let rec rec_search positions_to_check =
    match positions_to_check with
    | (position, direction) :: rest ->
      if not (Hash_set.mem visited position)
      then (
        Hash_set.add visited position;
        if Coordinate.equal position game.goal
        then Some [ direction ]
        else (
          let next_states = find_neighbors position game in
          match rec_search next_states with
          | Some path -> Some ([ direction ] @ path)
          | None -> rec_search rest))
      else rec_search rest
    | _ -> None
  in
  rec_search [ game.player, () ]
;;

let print_path path =
  List.iter path ~f:(fun dir ->
    match dir with
    | Direction.Left -> print_endline "Left"
    | Direction.Right -> print_endline "Right"
    | Direction.Up -> print_endline "Up"
    | Direction.Down -> print_endline "Down"
    | _ -> ())
;;

let solve_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file containing a maze and find a solution"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:"FILE a file containing a maze"
      in
      fun () ->
        let maze = In_channel.read_lines (File_path.to_string input_file) in
        let game = create_game maze in
        match find_path game with
        | Some path -> print_path path
        | None -> print_endline "Fail"]
;;

let command =
  Command.group ~summary:"maze commands" [ "solve", solve_command ]
;;
