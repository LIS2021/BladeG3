(** Program modeling the execution of Blade, usage:
    ./run_blade [--weights] [-o] [out.txt] <input.txt>
    [--weights]     : choose the model of weights between "simple" or "constant"
    [-o] [out.txt]  : using this flag dumps the result in the given file
**)
let usage_msg = "pipe [--blade] <file>"
let input_file = ref ""
let output_file = ref ""
let weight_model = ref ""
let spectre = ref false
let spec_list = [("--weights", Arg.Set_string weight_model, "Select weights model for blade");
                 ("-s1.1", Arg.Set spectre, "Enable protection vs Spectre1.1");
                 ("-o", Arg.Set_string output_file, "Save the processed source code in a file")]

let () =
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
          let final_ast = Blade.Blade.blade weights !spectre ast in
          if !output_file <> "" then
              (let out_file = open_out (!output_file ^ ".out") in
              (try output_string out_file (Ast.string_of_cmd final_ast);
              with e -> close_out_noerr out_file));
          Printf.printf "ast:\n\n%s\n" (Ast.string_of_cmd final_ast);
      | None -> failwith "cannot parse input file"
  with e ->
    close_in_noerr in_file;
    raise e;;
