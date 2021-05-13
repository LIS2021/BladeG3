module L = Llvm;;

let usage_msg = "llvm_eval.native <file>";;
let input_file = ref "";;
let enable_blade = ref false;;
let output_file = ref "";;
let spectre = ref false;;
let noifence = ref false;;
let verbose = ref false;;
let weight_model = ref "";;
let spec_list = [("--blade", Arg.Set enable_blade, "Enable blade optimization");
                 ("-s1.1", Arg.Set spectre, "Enable protection vs Spectre1.1");
                 ("--noifence", Arg.Set noifence, "Disable fence injection");
                 ("--weights", Arg.Set_string weight_model, "Select weights model for blade");
                 ("-v", Arg.Set verbose, "Enable verbose mode");
                 ("-o", Arg.Set_string output_file, "Save the processed ll code in a file")];;

let () =
  Arg.parse spec_list (fun s -> input_file := s) usage_msg;
  let in_file = open_in !input_file in
  try
    match Parser.parse_channel in_file with
      | Some(ast) ->
          let weights = (match !weight_model with
            | "simple" -> (module Blade.SimpleWeight : Blade.WeightModel)
            | "plainpr" -> (module Blade.PlainProtectWeight : Blade.WeightModel)
            | "constant"
            | _        -> (module Blade.ConstantWeight : Blade.WeightModel)) in
          let final_ast = if !enable_blade then Blade.Blade.blade weights !spectre ast else ast in
          (match Lleval.build_ir !verbose (not !noifence) final_ast with
            | Ok ir ->
                Printf.printf "%s\n" ir;
                if !output_file <> "" then
                  (let out_file = open_out (!output_file ^ ".ll") in
                  (try output_string out_file (ir);
                  with e -> close_out_noerr out_file));
             | Error e -> Printf.printf "error: %s\n" (Lleval.string_of_llerror e));
          L.dispose_module Lleval.lmodule
      | None -> failwith "Cannot parse input file"
  with e ->
    close_in_noerr in_file;
    raise e;;
