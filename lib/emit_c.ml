open Minimir
open Ast

let emit_function_header fname mir oc =
  failwith "todo"

let emit_struct_typedef struct_decl oc =
  failwith "todo"

let emit_c prog output_file =
  let oc = open_out output_file in
  
  (* emit boilerplate *)
  Printf.fprintf oc "#include <stdint.h>\n";
  Printf.fprintf oc "#include <stdbool.h>\n\n";
  
  (* Emit headers and store bodies *)
  Hashtbl.iter (fun _ decl ->
    match decl with
    | Dstruct d ->
        emit_struct_typedef d oc
    | Dfundef fd -> 
      let mir = Emit_minimir.emit_fun prog fd in 
      emit_function_header fd.fname.id mir oc
  ) prog;


  (* TODO: emit bodies *)

  
  close_out oc
