(** Program modeling the pipeline performed by the execution of virtual evaluator, usage:
    pipeline [--blade] [--model] [--weights] [-v] [-t] [-o] [out.txt] <input.txt>
    [--blade]       : using this flag enforce the use of blade
    [--model]       : choose the model of cost for the instructions between "simple", "fence" or "uniform"
    [--weights]     : choose the model of weights between "simple" or "constant"
    [-v]            : using this flag enables verbose output
    [-t] [out.txt]  : using this flag dumps the trace execution in the given file
    [-o] [out.txt]  : using this flag dumps the result in the given file
**)
let usage_msg = "pipe [OPTIONS] <file>"
let input_file = ref ""
let output_file = ref ""
let trace_file = ref ""
let enable_blade = ref false
let verbose = ref false
let vverbose = ref false
let cost_model = ref ""
let weight_model = ref ""
let speculator = ref ""
let spectre = ref false
let spec_list = [("--blade", Arg.Set enable_blade, "Enable blade optimization");
                 ("--model", Arg.Set_string cost_model, "Select cost model for evaluation");
                 ("--weights", Arg.Set_string weight_model, "Select weights model for blade");
                 ("--speculator", Arg.Set_string speculator, "Select the virtual machine speculation model");
                 ("-s1.1", Arg.Set spectre, "Enable protection vs Spectre1.1");
                 ("-v", Arg.Set verbose, "Enable verbose output");
                 ("-vv", Arg.Set vverbose, "Enable extra verbose output");
                 ("-t", Arg.Set_string trace_file, "Dumps the trace execution in a file");
                 ("-o", Arg.Set_string output_file, "Save the processed source code in a file")]

let () =
  Random.self_init ();
  Arg.parse spec_list (fun s -> input_file := s) usage_msg;
  let in_file = open_in !input_file in
  try
    match Parser.parse_channel in_file with
      | Some ast ->
          let weights = (match !weight_model with
            | "simple" -> (module Blade.SimpleWeight : Blade.WeightModel)
            | "plainpr" -> (module Blade.PlainProtectWeight : Blade.WeightModel)
            | "constant"
            | _        -> (module Blade.ConstantWeight : Blade.WeightModel)) in
          let final_ast = if !enable_blade then Blade.Blade.blade weights !spectre ast else ast in
          if !output_file <> "" then
              (let out_file = open_out (!output_file ^ ".out") in
              (try output_string out_file (Ast.string_of_cmd final_ast);
              with e -> close_out_noerr out_file));
          let conf = Evaluator.defaultConfiguration final_ast 100 in
          let spec = (match !speculator with
            | "outoforder" -> Evaluator.outOfOrderSpeculator !vverbose
            | "default"
            | _            -> Evaluator.defaultSpeculator Random.bool) in
          let cost = (match !cost_model with
            | "fence"  -> (module Evaluator.FenceSensitiveCost : Evaluator.CostModel)
            | "simple" -> (module Evaluator.SimpleCost : Evaluator.CostModel)
            | "fence2" -> (module Evaluator.FenceSpeculativeCost : Evaluator.CostModel)
            | "uniform"
            | _        -> (module Evaluator.UniformCost : Evaluator.CostModel)) in
          let eval = (if !trace_file <> "" then
            (fun conf spec cost ->
              let out_tr = (open_out (!trace_file ^ ".trace")) in
              (try Evaluator.evalWithTrace out_tr conf spec cost
              with e -> close_out_noerr out_tr; Evaluator.err Evaluator.EndOfStream))
            else Evaluator.eval) in
          Printf.printf "ast:\n\n%s\n" (Ast.string_of_cmd final_ast);
          (match eval conf spec cost with
            | Ok (conf', obs, count) ->
                if !verbose || !vverbose then
                  Printf.printf "\n%s" (Evaluator.string_of_verbose (conf', obs, count))
                else
                  Printf.printf "\ncount: %d\n" count
            | Error e -> Printf.printf "error: %s" (Evaluator.string_of_vmerror e))
      | None -> failwith "cannot parse input file"
  with e ->
    close_in_noerr in_file;
    raise e
