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
          let final_ast = if !enable_blade then Blade.blade ast else ast in
          let conf = {is = []; cs = [final_ast]; mu = Array.make 20 0; rho = StringMap.empty} in
          let spec = Evaluator.defaultSpeculator Rand.bool in
          (match Evaluator.eval conf Evaluator.UniformModel with
             | Ok (conf', obs, count) -> printf "count: %d\n" count
             | Err e -> printf "error: %s" (string_of_vmerror e))
      | None -> failwith "cannot parse input file"
  with e ->
    close_in_noerr in_file;
    raise e
