let generate_dot_file filename nodes edges =
  let oc = open_out filename in
  Printf.fprintf oc "digraph G {\n";
  List.iter (fun node -> Printf.fprintf oc "  %s;\n" node) nodes;
  List.iter (fun (src, dst) -> Printf.fprintf oc "  %s -> %s;\n" src dst) edges;
  Printf.fprintf oc "}\n";
  close_out oc
