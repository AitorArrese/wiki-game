open! Core
module City = String
module Highway = String

module Network = struct
  (* We can represent our social network graph as a set of connections, where
     a connection represents a friendship between two people. *)
  module Connection = struct
    module T = struct
      type t = Highway.t * City.t list [@@deriving compare, sexp]
    end

    let load_command =
      let open Command.Let_syntax in
      Command.basic
        ~summary:
          "parse a file listing interstates and serialize graph as a sexp"
        [%map_open
          let input_file =
            flag
              "input"
              (required File_path.arg_type)
              ~doc:
                "FILE a file listing interstates and the cities they go \
                 through"
          in
          fun () ->
            ignore (input_file : File_path.t);
            failwith "TODO"]
    ;;

    let rec make_pairs highway list =
      match list with
      | a :: rest ->
        List.map rest ~f:(fun b -> a, b) @ make_pairs highway rest
      | _ -> []
    ;;

    let make_connections highway_connections =
      let connected_places = String.split highway_connections ~on:',' in
      match connected_places with
      | highway :: cities -> make_pairs highway cities
      | _ -> []
    ;;

    let of_string s =
      let highway_connections = String.split s ~on:'\n' in
      List.map highway_connections ~f:make_connections
    ;;
  end

  let visualize_command =
    let open Command.Let_syntax in
    Command.basic
      ~summary:
        "parse a file listing interstates and generate a graph visualizing \
         the highway network"
      [%map_open
        let input_file =
          flag
            "input"
            (required File_path.arg_type)
            ~doc:
              "FILE a file listing all interstates and the cities they go \
               through"
        and output_file =
          flag
            "output"
            (required File_path.arg_type)
            ~doc:"FILE where to write generated graph"
        in
        fun () ->
          ignore (input_file : File_path.t);
          ignore (output_file : File_path.t);
          printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
  ;;

  let command =
    Command.group
      ~summary:"interstate highway commands"
      [ "load", load_command; "visualize", visualize_command ]
  ;;
end
