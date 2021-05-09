module L = Llvm;;

let usage_msg = "llvm_eval.native <file>";;
let input_file = ref "";;
let enable_blade = ref false;;
let spectre = ref false;;
let fancy = ref false;;
let weight_model = ref "";;
let spec_list = [("--blade", Arg.Set enable_blade, "Enable blade optimization");
                 ("-s1.1", Arg.Set spectre, "Enable protection vs Spectre1.1");
                 ("--fancy", Arg.Set fancy, "Enable fence injection");
                 ("--weights", Arg.Set_string weight_model, "Select weights model for blade")];;

let () =
  Arg.parse spec_list (fun s -> input_file := s) usage_msg;
  let in_file = open_in !input_file in
  try
    match Parser.parse_channel in_file with
      | Some(ast) ->
          let weights = (match !weight_model with
            | "simple" -> (module Blade.SimpleWeight : Blade.WeightModel)
            | "constant"
            | _        -> (module Blade.ConstantWeight : Blade.WeightModel)) in
          let final_ast = if !enable_blade then Blade.Blade.blade weights !spectre ast else ast in
          let ir = Lleval.build_ir final_ast !fancy in
          Printf.printf "%s\n" ir;
          L.dispose_module Lleval.lmodule
      | None -> failwith "Parsing error"
  with e ->
    close_in_noerr in_file;
    raise e   

