
let usage_msg = "pipeline [--blade] <file>"
let input_file = ref ""
let output_file = ref ""
let trace_file = ref ""
let enable_blade = ref false
let verbose = ref false
let cost_model = ref ""
let wieght_model = ref ""
let spec_list = [("--blade", Arg.Set enable_blade, "Enable blade optimization");
                 ("--model", Arg.Set_string cost_model, "Select cost model for evaluation"),
                 ("--weights", Arg.Set_string weight_model, "Select weights model for blade"),
                 ("-v", Arg.Set verbose, "Enable verbose output"),
                 ("-t", Arg.Set_string trace_file, "Dumps the trace execution in a file"),
                 ("-o", Arg.Set_string output_file, "Save the processed source code in a file")]

let () =
  Arg.parse spec_list (fun s -> input_file := s) usage_msg;
  try
    let in_file = open_in !input_file in
    match Parser.parse_channel in_file with
      | Some ast ->
          let weights = (match !weight_model with
            | _ -> (module Blade.ConstantWeight : Blade.WeightModel)) in
          let final_ast = if !enable_blade then Blade.Blade.blade weights ast else ast in
          if !output_file <> "" then output_string (open_out !out_file) (Ast.string_of_cmd final_ast);
          let conf = Evaluator.defaultConfiguration final_ast 100 in
          let spec = Evaluator.defaultSpeculator Random.bool in
          let cost = (match !cost_model with
            | "fence" ->  (module Evaluator.FenceSensitiveCost : Evaluator.CostModel)
            | "simple" -> (module Evaluator.SimpleCost : Evaluator.CostModel)
            | "uniform"
            | _       ->  (module Evaluator.UniformCost : Evaluator.CostModel)) in
          let eval = if !trace_file <> "" then Evaluator.evalWithTrace (open_out !trace_file) else Evaluator.eval in
          Printf.printf "ast: %s\n" (Ast.string_of_cmd final_ast);
          (match Evaluator.eval conf spec cost with
             | Ok (conf', obs, count) -> Printf.printf "count: %d\n" count
             | Error e -> Printf.printf "error: %s" (Evaluator.string_of_vmerror e))
      | None -> failwith "cannot parse input file"
  with e ->
    close_in_noerr in_file;
    raise e
