open Minimir
open Ast

let rec ctyp_of_mirtyp typ =
  match typ with
    | Tstruct (struct_name, _) -> struct_name
    | Tborrow (_, mut, borrow_type) -> (
        match mut with
        | Mut -> Printf.sprintf "%s*" (ctyp_of_mirtyp borrow_type)
        | NotMut ->  Printf.sprintf "const %s*" (ctyp_of_mirtyp borrow_type)
      )

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
  

let emit_struct_header struct decl oc =
  

let emit_struct_typedef struct_decl oc =

  Printf.fprintf oc "typedef struct {\n";
  List.iter (
    fun (field_name,field_type) ->
      Printf.fprintf oc "    %s %s;\n" (ctyp_of_mirtyp field_type) field_name.id
  ) struct_decl.sfields;

  Printf.fprintf oc "} %s;\n\n" struct_decl.sname.id

let emit_c prog output_file =
  let oc = open_out output_file in
  
  (* emit boilerplate *)
  Printf.fprintf oc "#include <stdint.h>\n";
  Printf.fprintf oc "#include <stdbool.h>\n\n";
  
  (* Emit struct typedefs first*)
  Hashtbl.iter (fun _ decl ->
    match decl with
    | Dstruct d ->
        emit_struct_typedef d oc
    | Dfundef _ ->  ()
  ) prog;

  (* The emit functions prototypes after struct typedefs because we might need them *)
  (* We do prototypes first and not function implementations directly because i don't want to deal with function order *)
  let mir_bodies = Hashtbl.fold (fun _ decl acc ->
    match decl with
    | Dstruct _ -> acc
    | Dfundef fd ->
      let mir = Emit_minimir.emit_fun prog fd in 
      emit_function_header fd.fname.id mir oc;
    mir :: acc
  ) prog [] in

  let _ = mir_bodies in (*todo, function implem*)

  


  (* TODO: emit bodies *)

  
  close_out oc
