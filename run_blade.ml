(** Program modeling the pipeline performed by the execution of Blade, usage:
    pipeline [--blade] [--model] [--weights] [-v] [-t] [-o] [out.txt] <input.txt>
    [--blade]       : using this flag enforce the use of blade
    [--model]       : choose the model of cost for the instructions between "simple", "fence" or "uniform"
    [--weights]     : choose the model of weights between "simple" or "constant"
    [-v]            : using this flag enables verbose output
    [-t] [out.txt]  : using this flag dumps the trace execution in the given file
    [-o] [out.txt]  : using this flag dumps the result in the given file 
**)
let usage_msg = "pipe [--blade] <file>"
let input_file = ref ""
let output_file = ref ""
let weight_model = ref ""
let spec_list = [("--weights", Arg.Set_string weight_model, "Select weights model for blade");
                 ("-o", Arg.Set_string output_file, "Save the processed source code in a file")]

let () =
  Arg.parse spec_list (fun s -> input_file := s) usage_msg;
  let in_file = open_in !input_file in
  try
    match Parser.parse_channel in_file with
      | Some ast ->
          let weights = (match !weight_model with
            | "simple" -> (module Blade.SimpleWeight : Blade.WeightModel)
            | "constant"
            | _        -> (module Blade.ConstantWeight : Blade.WeightModel)) in
          let final_ast = Blade.Blade.blade weights ast in
          if !output_file <> "" then 
              (let out_file = open_out (!output_file ^ ".out") in
              (try output_string out_file (Ast.string_of_cmd final_ast);
              with e -> close_out_noerr out_file));
          Printf.printf "ast:\n\n%s\n" (Ast.string_of_cmd final_ast);
      | None -> failwith "cannot parse input file"
  with e ->
    close_in_noerr in_file;
    raise e