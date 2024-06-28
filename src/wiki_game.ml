open! Core

let get_linked_articles contents : string list =
  let is_not_namespace link =
    match Wikipedia_namespace.namespace link with
    | None -> true
    | Some _namespace -> false
  in
  let open Soup in
  parse contents
  $$ "a[href*=/wiki/]"
  |> to_list
  |> List.map ~f:(fun a -> R.attribute "href" a)
  |> List.filter ~f:is_not_namespace
  |> List.dedup_and_sort ~compare:String.compare
;;

let%expect_test "get_linked_articles" =
  (* This test uses existing files on the filesystem. *)
  let contents =
    File_fetcher.fetch_exn
      (Local (File_path.of_string "../resources/wiki"))
      ~resource:"Cat"
  in
  List.iter (get_linked_articles contents) ~f:print_endline;
  [%expect
    {|
  /wiki/Carnivore
  /wiki/Domestication_of_the_cat
  /wiki/Mammal
  /wiki/Species
  |}]
;;

let print_links_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Print all of the valid wiki page links on a page"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_linked_articles contents) ~f:print_endline]
;;

(* [visualize] should explore all linked articles up to a distance of
   [max_depth] away from the given [origin] article, and output the result as
   a DOT file. It should use the [how_to_fetch] argument along with
   [File_fetcher] to fetch the articles so that the implementation can be
   tested locally on the small dataset in the ../resources/wiki directory. *)
module Article = String
module G = Graph.Imperative.Graph.Concrete (Article)

(* We extend our [Graph] structure with the [Dot] API so that we can easily
   render constructed graphs. Documentation about this API can be found here:
   https://github.com/backtracking/ocamlgraph/blob/master/src/dot.mli *)
module Dot = Graph.Graphviz.Dot (struct
    include G

    (* These functions can be changed to tweak the appearance of the
       generated graph. Check out the ocamlgraph graphviz API
       (https://github.com/backtracking/ocamlgraph/blob/master/src/graphviz.mli)
       for examples of what values can be set here. *)
    let edge_attributes _ = [ `Dir `None ]
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
    let vertex_name v = sprintf {|"%s"|} v
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)

let create_article_name str =
  let str_title =
    List.hd (List.rev (String.split_on_chars str ~on:[ '/' ]))
  in
  match str_title with Some title -> title | None -> "EMPTY"
;;

let visualize ?(max_depth = 3) ~origin ~output_file ~how_to_fetch () : unit =
  let graph = G.create () in
  let visited = String.Hash_set.create () in
  let rec find_articles ~depth articles_to_check =
    match articles_to_check with
    | first :: rest ->
      let article = create_article_name first in
      if not (Hash_set.mem visited article)
      then (
        print_s [%message (first : string)];
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource:first in
        Hash_set.add visited article;
        let next_level = get_linked_articles contents in
        List.iter next_level ~f:(fun child_link ->
          G.add_edge graph article (create_article_name child_link));
        find_articles ~depth rest;
        match depth with
        | 0 -> ()
        | _ -> find_articles ~depth:(depth - 1) next_level)
      else find_articles ~depth rest
    | _ -> ()
  in
  find_articles ~depth:(max_depth + 1) [ origin ];
  Dot.output_graph
    (Out_channel.create (File_path.to_string output_file))
    graph
;;

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing \
       the highway network"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        visualize ~max_depth ~origin ~output_file ~how_to_fetch ();
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

(* [find_path] should attempt to find a path between the origin article and
   the destination article via linked articles.

   [find_path] should use the [how_to_fetch] argument along with
   [File_fetcher] to fetch the articles so that the implementation can be
   tested locally on the small dataset in the ../resources/wiki directory.

   [max_depth] is useful to limit the time the program spends exploring the
   graph. *)

module Connection = struct
  module T = struct
    type t =
      { article1 : string
      ; article2 : string
      }
    [@@deriving compare, sexp, hash, equal]
  end

  include Comparable.Make (T)
  include T
end

let make_url url =
  if String.equal (String.prefix url 6) "https:"
  then "%"
  else "https://en.wikipedia.org" ^ url
;;

(*let find_path ?(max_depth = 3) ~origin ~destination ~how_to_fetch () = let
  visited = String.Hash_set.create () in let rec rec_search
  positions_to_check depth = let solution = List.concat_map
  positions_to_check ~f:(fun (article, path) -> if String.equal article
  destination then [ path ] else []) in if List.is_empty solution then ( let
  next_gen = List.concat_map positions_to_check ~f:(fun (article, path) -> if
  Hash_set.mem visited article then [] else ( Hash_set.add visited article;
  let contents = File_fetcher.fetch_exn how_to_fetch ~resource:article in
  get_linked_articles contents |> List.map ~f:(fun article -> make_url
  article, path @ [ create_article_name article ]))) in rec_search next_gen
  (depth - 1)) else List.hd solution in rec_search [ origin, [
  create_article_name origin ] ] max_depth ;;*)

(*let find_path ?(max_depth = 3) ~origin ~destination ~how_to_fetch () = let
  visited = String.Hash_set.create () in let rec rec_search
  positions_to_check depth = match List.find_map positions_to_check ~f:(fun
  (article, path) -> if String.equal article destination then Some path else
  None) with | Some path -> path | None -> let next_gen = List.concat_map
  positions_to_check ~f:(fun (article, path) -> if Hash_set.mem visited
  article then [] else ( Hash_set.add visited article; let contents =
  File_fetcher.fetch_exn how_to_fetch ~resource:article in
  get_linked_articles contents |> List.map ~f:(fun article -> make_url
  article, path @ [ create_article_name article ]))) in rec_search next_gen
  (depth - 1) in rec_search [ origin, [ create_article_name origin ] ]
  max_depth ;;*)

(*let find_path ?(max_depth = 3) ~origin ~destination ~how_to_fetch () = let
  visited = String.Hash_set.create () in let parent = Hashtbl.create (module
  String) in let rec rec_search positions_to_check = match List.find_map
  positions_to_check ~f:(fun article -> if String.equal article destination
  then Some article else None) with | Some path -> path | None -> let
  next_gen = List.concat_map positions_to_check ~f:(fun article -> if
  Hash_set.mem visited article then [] else ( Hash_set.add visited article;
  let contents = File_fetcher.fetch_exn how_to_fetch ~resource:article in
  get_linked_articles contents |> List.map ~f:(fun article2 -> if not
  (Hashtbl.mem parent (make_url article2)) then Hashtbl.add_exn parent
  ~key:(make_url article2) ~data:article; make_url article2))) in let _x =
  max_depth in rec_search next_gen in let rec find_traveral path = if
  String.equal path origin then [ origin ] else find_traveral
  (Hashtbl.find_exn parent path) @ [ path ] in find_traveral (rec_search [
  origin ]) ;;*)

let find_path ?(max_depth = 3) ~origin ~destination ~how_to_fetch () =
  let visited = String.Hash_set.create () in
  let parent = Hashtbl.create (module String) in
  let found = ref false in
  let rec rec_search positions_to_check =
    List.iter positions_to_check ~f:(fun article ->
      if String.equal article destination then found := true);
    if not !found
    then (
      let next_gen =
        List.concat_map positions_to_check ~f:(fun article ->
          if Hash_set.mem visited article
             || String.contains article '%'
             || String.contains article ')'
          then []
          else (
            print_s [%message (article : string)];
            Hash_set.add visited article;
            let contents =
              File_fetcher.fetch_exn how_to_fetch ~resource:article
            in
            get_linked_articles contents
            |> List.map ~f:(fun article2 ->
              let url = make_url article2 in
              if not (Hashtbl.mem parent url)
              then
                Hashtbl.add_exn parent ~key:(make_url article2) ~data:article;
              make_url article2)))
      in
      let _x = max_depth in
      rec_search next_gen)
  in
  let rec find_traveral path =
    if String.equal path origin
    then [ origin ]
    else find_traveral (Hashtbl.find_exn parent path) @ [ path ]
  in
  rec_search [ origin ];
  find_traveral destination
;;

let find_path_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "Play wiki game by finding a link between the origin and destination \
       pages"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and destination =
        flag "destination" (required string) ~doc:" the destination page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      in
      let start_time = Time_now.nanoseconds_since_unix_epoch () in
      fun () ->
        let path =
          find_path ~max_depth ~origin ~destination ~how_to_fetch ()
        in
        let end_time = Time_now.nanoseconds_since_unix_epoch () in
        let total = Base.Int63.( - ) end_time start_time in
        print_s [%message (total : Int63.t)];
        List.iter path ~f:print_endline]
;;

(* match find_path ~max_depth ~origin ~destination ~how_to_fetch () with |
   None -> print_endline "No path found!" | Some trace -> let end_time =
   Time_now.nanoseconds_since_unix_epoch () in let total = Base.Int63.( - )
   end_time start_time in print_s [%message (total : Int63.t)]; List.iter
   trace ~f:print_endline] *)

let command =
  Command.group
    ~summary:"wikipedia game commands"
    [ "print-links", print_links_command
    ; "visualize", visualize_command
    ; "find-path", find_path_command
    ]
;;
