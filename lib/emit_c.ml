open Minimir
open Ast

let ctyp_of_mirtyp typ =
  match typ with
    | Tstruct (_, _) -> failwith "todo: add ctyp_of_mirtyp struct"
    | Tborrow (_, _, _) -> failwith "todo: add ctyp_of_mirtyp borrow"
    | Tunit -> "void"
    | Ti32 -> "int32_t"
    | Tbool -> "bool"
  

let emit_function_header fname mir oc =
  (* Ici on ignore les variables locales pour l'instant *)
  let args = ref [] in
  let ret_type = ref Tunit in

  Hashtbl.iter(fun local this_typ -> 
    match local with
    | Lparam param_name -> args := (param_name, this_typ) :: !args;      
    | Lvar _ ->  ()
    | Lret -> ret_type := this_typ
  ) mir.mlocals;

  (*Emit ret type and fname*)
  Printf.fprintf oc "%s %s" (ctyp_of_mirtyp !ret_type) fname;
  
  (*Emit args*)
  Printf.fprintf oc "(";
  List.iteri (
    fun i (arg_name,arg_type) -> 
      Printf.fprintf oc "%s %s" (ctyp_of_mirtyp arg_type) arg_name;
      if i < (List.length !args)-1 then(
        Printf.fprintf oc ", "
      );
  ) !args;
  
  Printf.fprintf oc ");\n"
  


let emit_struct_typedef struct_decl oc =
  let _ = (struct_decl, oc) in
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
