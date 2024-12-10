(** print NODE to STREAM in Dot format *)
let print_node node stream =
  Printf.fprintf stream "n%d " node

(** print ARC to STREAM in Dot format *)
let print_arc arc stream =
let o, e = arc_origin arc, arc_extremity arc in
begin
  Printf.fprintf stream "n%d -> n%d" o e;
  Printf.fprintf stream ";\n";
end

(** print a set of arcs ARCS to STREAM in Dot format *)
let print_arcs arcs stream =
List.iter (fun arc -> print_arc arc stream) arcs

(** print a set of nodes NODES to STREAM in Dot format *)
let print_nodes nodes stream =
List.iter (fun node ->
    begin
      print_node node stream;
      Printf.fprintf stream ";\n" ;
    end)
  nodes

let graph_name graph = "mygraph"

let out_name name = "g_" ^ name
let dot_name name = out_name name ^ ".dot"
let pdf_name name = out_name name ^ ".pdf"

(** print GRAPH to STREAM in Dot format *)
let graph_to_dot graph stream =
begin
  Printf.fprintf stream "digraph \"%s\" {\n" (graph_name graph);
  print_nodes (graph_nodes graph) stream;
  print_arcs (graph_arcs graph) stream;
  Printf.fprintf stream "}\n";
end

(** save GRAPH to file with name depending on the name of the graph *)
let graph_to_dot_file graph =
let stream = open_out (dot_name (graph_name graph)) in
begin
  graph_to_dot graph stream;
  close_out stream;
end

(** convert a Dot file to pdf format for future viewing *)
let dot_to_pdf name =
ignore (Sys.command ("dot -Tpdf " ^ dot_name name ^ " -o " ^ pdf_name name))

(** visualize GRAPH *)
let graph_view graph =
let name = graph_name graph in
begin
  graph_to_dot_file graph;
  dot_to_pdf name;
  ignore (Sys.command ("evince " ^ pdf_name name ^ "&"));
end