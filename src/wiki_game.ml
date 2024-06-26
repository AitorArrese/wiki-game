open! Core

(* [get_linked_articles] should return a list of wikipedia article lengths contained in
   the input.

   Note that [get_linked_articles] should ONLY return things that look like wikipedia
   articles. In particular, we should discard links that are:
   - Wikipedia pages under special namespaces that are not articles (see
     https://en.wikipedia.org/wiki/Wikipedia:Namespaces)
   - other Wikipedia internal URLs that are not articles
   - resources that are external to Wikipedia
   - page headers

   One nice think about Wikipedia is that stringent content moderation results in
   uniformity in article format. We can expect that all Wikipedia article links parsed
   from a Wikipedia page will have the form "/wiki/<TITLE>". *)

   let get_linked_articles contents : string list =
      let is_not_namespace link = 
        match Wikipedia_namespace.namespace link with
      |None -> true
      |Some _namespace -> false
      in
  let open Soup in
  parse contents
  $$ "a[href*=/wiki/]"
  |> to_list
  |> List.map ~f:(fun a-> (R.attribute "href" a)) |> List.filter ~f:is_not_namespace |> List.dedup_and_sort ~compare:String.compare
;;

let%expect_test "get_linked_articles" =
  (* This test uses existing files on the filesystem. *)
  let contents =
    File_fetcher.fetch_exn
      (Local (File_path.of_string "../resources/wiki"))
      ~resource:"Carnivore"
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

(* [visualize] should explore all linked articles up to a distance of [max_depth] away
   from the given [origin] article, and output the result as a DOT file. It should use the
   [how_to_fetch] argument along with [File_fetcher] to fetch the articles so that the
   implementation can be tested locally on the small dataset in the ../resources/wiki
   directory. *)
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

let create_article_name str= 
     let str_title = List.hd (List. rev(String.split_on_chars str ~on: ['/'] )) in
     match str_title with
     | Some title -> title
     | None -> "EMPTY"
     
let visualize ?(max_depth = 3) ~origin ~output_file ~how_to_fetch () : unit =
  let graph = G.create () in
  let visited = String.Hash_set.create () in
  let rec find_articles ~depth articles_to_check =
    match articles_to_check with
  | first :: rest ->
    let article = create_article_name first in
    if not (Hash_set.mem visited article) 
      then
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource:first in
        Hash_set.add visited article;
        let next_level = get_linked_articles contents in
        List.iter next_level ~f:(fun child_link -> 
          G.add_edge graph article (create_article_name child_link));
        find_articles ~depth rest;
        (match depth with
        | 0 -> ()
        | _ -> find_articles ~depth:(depth -1) next_level
        )
        else find_articles ~depth rest;
  | _ -> ()
  in

  find_articles ~depth:(max_depth+1) [origin];
  Dot.output_graph (Out_channel.create (File_path.to_string output_file)) graph;
;;

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing the highway \
       network"
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

(* [find_path] should attempt to find a path between the origin article and the
   destination article via linked articles.

   [find_path] should use the [how_to_fetch] argument along with [File_fetcher] to fetch
   the articles so that the implementation can be tested locally on the small dataset in
   the ../resources/wiki directory.

   [max_depth] is useful to limit the time the program spends exploring the graph. *)

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

(*
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

*)
type connection = {
  article1: string;
  article2: string
}


let find_path ?(max_depth = 3) ~origin ~destination ~how_to_fetch () =
  ignore (max_depth : int);
  ignore (origin : string);
  ignore (destination : string);
  ignore (how_to_fetch : File_fetcher.How_to_fetch.t);
  let visited = String.Hash_set.create () in
  let rec rec_search positions_to_check = 
    match positions_to_check with 
    | (article, connection) :: rest ->
      let article_name = create_article_name article in
      if not (Hash_set.mem visited article_name) then (
        Hash_set.add visited article_name;
      let contents = File_fetcher.fetch_exn how_to_fetch ~resource:article in
      if String.equal article_name destination then Some [connection]
      else (
        let next_sites = get_linked_articles contents|> List.map ~f:(fun article2 -> (article2, {article1 = article_name; article2})) in
        match rec_search (rest @ next_sites) with
        |Some path -> Some ([connection] @ path)
        |None -> None))
    else rec_search rest
    | _ -> None
  in
  rec_search [(origin, {article1 = "Empty"; article2 = "Empty"})]
;;

let find_path_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Play wiki game by finding a link between the origin and destination pages"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and destination = flag "destination" (required string) ~doc:" the destination page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      in
      fun () ->
        match find_path ~max_depth ~origin ~destination ~how_to_fetch () with
        | None -> print_endline "No path found!"
        | Some trace -> List.iter trace ~f:(fun connection -> 
          printf !"%{String} <--> %{String}" connection.article1 connection.article2) ]
;;

let command =
  Command.group
    ~summary:"wikipedia game commands"
    [ "print-links", print_links_command
    ; "visualize", visualize_command
    ; "find-path", find_path_command
    ]
;;
