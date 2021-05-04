
let usage_msg = "pipeline [--blade] <file>"
let input_file = ref ""
let enable_blade = ref false
let cost_model = ref ""
let spec_list = [("--blade", Arg.Set enable_blade, "Enable blade optimization");
                 ("--model", Arg.Set_string cost_model, "Select cost model for evaluation")]

let () =
  Arg.parse spec_list (fun s -> input_file := s) usage_msg;
  let in_file = open_in !input_file in
  try
    match Parser.parse_channel in_file with
      | Some ast ->
          let final_ast = if !enable_blade then Blade.Blade.blade ast else ast in
          let conf = Evaluator.defaultConfiguration final_ast 100 in
          let spec = Evaluator.defaultSpeculator Random.bool in
          let cost = (match !cost_model with
            | "fence" ->  (module Evaluator.FenceSensitiveCost : Evaluator.CostModel)
            | "simple" -> (module Evaluator.SimpleCost : Evaluator.CostModel)
            | "uniform"
            | _       ->  (module Evaluator.UniformCost : Evaluator.CostModel)) in
          Printf.printf "ast: %s\n" (Ast.string_of_cmd final_ast);
          (match Evaluator.eval conf spec cost with
             | Ok (conf', obs, count) -> Printf.printf "count: %d\n" count
             | Error e -> Printf.printf "error: %s" (Evaluator.string_of_vmerror e))
      | None -> failwith "cannot parse input file"
  with e ->
    close_in_noerr in_file;
    raise e
