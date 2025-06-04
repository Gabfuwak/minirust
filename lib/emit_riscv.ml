open Minimir
open Ast

let indent = "  "

let _size_of_typ typ =
  match typ with
  | Tbool -> 1
  | Tunit -> 0
  | Ti32 -> 4
  | Tstruct (_, _) -> failwith "todo"
  | Tborrow _ -> 4

let riscv_of_rvalue rval =
  match rval with
  | RVunit -> "" 
  | _ -> ""

let riskv_of_place _pl =
  failwith "todo"

let emit_function prog fundef mir_body oc = 
  (* emit definitions de locales Note: remplacer ça par autre chose?*)
  Printf.fprintf oc "%s:\n" fundef.fname.id;

  Array.iteri (
    fun curr_lbl (instr, _)->
    (*TODO possible: preprocess quels labels vont etre utilisés pour ne generer que ceux la*)
    Printf.fprintf oc "L%d:\n" curr_lbl;
    match instr with
    | Iassign (_, RVunit, next_lbl) -> 
        if next_lbl = curr_lbl + 1 then
          () (* No-op, c'est séquentiel *)
        else
          (* Avec l'implementation actuelle de minimir je crois que ça devrait etre unreachable mais autant etre robuste *)
          Printf.fprintf oc "%sj L%d\n" indent next_lbl 
    | Iassign (pl_dest, rval, _) -> 
        ignore @@ failwith "todo";
        Printf.fprintf oc "%s%s = %s;\n" indent (riskv_of_place pl_dest) (riscv_of_rvalue rval);
    | Icall (name, args, retplace, _) ->
        ignore @@ failwith "todo";
        Printf.fprintf oc "%s" indent;
        let ret_type = typ_of_place prog mir_body retplace in
        if ret_type <> Tunit then
          Printf.fprintf oc "%s = " (riskv_of_place retplace) 
        ;
        Printf.fprintf oc "%s(" name;
        List.iteri (
          fun i arg_pl ->
            if i < (List.length args) - 1 then
              Printf.fprintf oc "%s, " (riskv_of_place arg_pl)
            else
              Printf.fprintf oc "%s" (riskv_of_place arg_pl)
        ) args;
        Printf.fprintf oc ");\n";

    | Igoto (dest_lbl) -> 
        ignore @@ failwith "todo";
        Printf.fprintf oc "%sgoto L%d;\n" indent dest_lbl
    | Iif (place_to_check, then_lbl, else_lbl) -> 
        ignore @@ failwith "todo";
        (* un peu overkill, ça pourrait se faire en une ligne mais je pense que ce sera plus simple pour faire de l'idiomatique ensuite comme ça *)
        Printf.fprintf oc "%sif (%s) {\n" indent (riskv_of_place place_to_check);
        Printf.fprintf oc "%sgoto L%d;\n" indent then_lbl;
        Printf.fprintf  oc "%s} else {\n" indent;
        Printf.fprintf oc "%sgoto L%d;\n" indent else_lbl;
        Printf.fprintf  oc "%s}\n" indent;
        
        
    | Ireturn ->
        let ret_type = Hashtbl.find mir_body.mlocals Lret in
        if ret_type = Tunit then
          Printf.fprintf oc "%s# rien pour _ret = ()\n" indent
        else
          failwith "todo"
        ;
        Printf.fprintf oc "%sjr ra\n" indent 
    | Ideinit _ -> () (* rien a faire *)
  ) mir_body.minstrs;

  ()
  
  


let emit_riskv prog output_file =
  let oc = open_out output_file in
  
  (* emit boilerplate *)
  Printf.fprintf oc ".text\n.global _start\n";


  Printf.fprintf oc "_start:\n";
  (* appel a la fonction main *)
  Printf.fprintf oc "%sjal ra, main\n\n" indent;

  (* System call exit *)
  Printf.fprintf oc "%sli a7, 93\n" indent;   
  Printf.fprintf oc "%sli a0, 0\n" indent;   
  Printf.fprintf oc "%secall\n" indent;   


  (* The emit functions prototypes after struct typedefs because we might need them *)
  Hashtbl.iter (fun _ decl->
    match decl with
    | Dstruct _ -> failwith "todo"
    | Dfundef fd ->
      let mir = Emit_minimir.emit_fun prog fd in 
      emit_function prog fd mir oc;
  ) prog;

  (*Si on a pas de main on en declare un vide a la main et le reste n'est jamais appelé. Autrement l'assembleur ne compile (assemble?) pas*)
  let has_main = Hashtbl.mem prog "main" in
  if not has_main then (
    Printf.fprintf oc "main:\n";
    Printf.fprintf oc "%sjr ra\n" indent
  );

  close_out oc

