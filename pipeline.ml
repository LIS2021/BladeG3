
let usage_msg = "pipeline [--blade] <file>"
let input_file = ref ""
let enable_blade = ref false
let spec_list = [("--blade", Arg.Set enable_blade, "Enable blade optimization")]

let () =
  Arg.parse spec_list (fun s -> input_file := s) usage_msg;
  let in_file = open_in !input_file in
  try
    match Parser.parse_channel in_file with
      | Some ast ->
          let final_ast = if !enable_blade then Blade.Blade.blade ast else ast in
          let conf = Evaluator.defaultConfiguration final_ast 20 in
          let spec = Evaluator.defaultSpeculator Random.bool in
          let cost = (module Evaluator.UniformCost : Evaluator.CostModel) in
          (match Evaluator.eval conf spec cost with
             | Ok (conf', obs, count) -> Printf.printf "ast: %s\n\ncount: %d\n" (Ast.string_of_cmd final_ast) count
             | Error e -> Printf.printf "error: %s" (Evaluator.string_of_vmerror e))
      | None -> failwith "cannot parse input file"
  with e ->
    close_in_noerr in_file;
    raise e
