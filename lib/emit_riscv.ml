open Minimir
open Ast

let indent = "  "

let label_name fundef lbl = 
  Printf.sprintf "%s_L%d" fundef.fname.id lbl

let string_of_instr instr =
  Format.asprintf "%a" Print_minimir.pp_instr instr

let size_of_typ typ =
  match typ with
  | Tbool -> 1
  | Tunit -> 0
  | Ti32 -> 4
  | Tstruct (_, _) -> failwith "todo"
  | Tborrow _ -> 4

let riscv_of_rvalue rval =
  match rval with
  | RVunit -> "" 
  | RVconst (Cbool true) -> "1"
  | RVconst (Cbool false) -> "0"
  | RVconst (Ci32 a) ->
      (* On enleve le suffixe i32 du litteral *)
      let len = String.length a in
      String.sub a 0 (len - 3)
  | _ -> failwith "todo"


let compute_all_offsets mir_body =
  let offset_table = Hashtbl.create 16 in
  let current_offset = ref 0 in
  
  Hashtbl.iter (fun local typ ->
    match local with
    | Lvar id ->
        let size = size_of_typ typ in
        let aligned_size = ((size + 3) / 4) * 4 in
        current_offset := !current_offset + aligned_size;
        Hashtbl.add offset_table id (!current_offset)
    | Lparam _ -> () (* a0-a7, TODO: ajouter le calcul correct pour generer le cas avec plus de 8 paramètres *)
    | Lret ->
        let size = size_of_typ typ in
        let aligned_size = ((size + 3) / 4) * 4 in
        current_offset := !current_offset + aligned_size;
        Hashtbl.add offset_table (-1) (!current_offset) (* on met -1 comme id special pour la valeur de retour, on est sur que ça ne sera jamais pris *)
  ) mir_body.mlocals;
  
  (!current_offset, offset_table)

let riscv_of_place offset_table pl =
  match pl with
  | PlLocal (Lvar id) -> 
      let offset = Hashtbl.find offset_table id in
      Printf.sprintf "-%d(sp)" offset
  | PlLocal (Lparam _name) -> 
      "a0" (* TODO: mapping correct des paramètres *)
  | PlLocal Lret -> 
      let offset = Hashtbl.find offset_table (-1) in
      Printf.sprintf "-%d(sp)" offset
  | PlDeref _ -> failwith "todo"
  | PlField _ ->  failwith "todo"

let goto_next fundef oc curr_lbl next_lbl =
  if next_lbl = curr_lbl + 1 then
    () (* No-op, c'est séquentiel *)
  else
    (* Avec l'implementation actuelle de minimir je crois que ça devrait etre unreachable mais autant etre robuste *)
    Printf.fprintf oc "%sj %s\n" indent (label_name fundef next_lbl) 

let emit_function prog fundef mir_body oc = 

  let (_curr_offset, offset_table) = compute_all_offsets mir_body in
  let frame_size = 64 in (*TODO: faire un vrai calcul*)

  (* emit definitions de locales Note: remplacer ça par autre chose?*)
  Printf.fprintf oc "%s:\n" fundef.fname.id;

  (* allouer la stack *)
  Printf.fprintf oc "%saddi sp, sp, -%d\n" indent frame_size;

  (* On stocke ra a la fin de la frame *)
  (* Techniquement c'est une inneficience et on devrait verifier si la fonction fait des Icall mais en vrai c'est pas tres cher (maybe TODO quand j'en serais aux optis?) *)
  Printf.fprintf oc "%ssw ra, %d(sp)\n" indent (frame_size - 4);

  Array.iteri (
    fun curr_lbl (instr, _)->
    (*TODO possible: preprocess quels labels vont etre utilisés pour ne generer que ceux la*)
    Printf.fprintf oc "%s:\n" (label_name fundef curr_lbl);
    Printf.fprintf oc "%s# %s\n" indent (string_of_instr instr); (*Debug/lisibilité ou on met l'instruction minimir dans l'assembleur en commentaire*)
    match instr with
    | Iassign (_, RVunit, next_lbl) -> 
        goto_next fundef oc curr_lbl next_lbl
    | Iassign (pl_dest, rval, next_lbl) -> 
        Printf.fprintf oc "%sli t0, %s\n" indent (riscv_of_rvalue rval);
        Printf.fprintf oc "%ssw t0, %s\n" indent (riscv_of_place offset_table pl_dest);
        goto_next fundef oc curr_lbl next_lbl
    | Icall (name, args, retplace, next_lbl) ->
        List.iteri (
          fun i arg_pl ->
            if i < 8 then(
              Printf.fprintf oc "%slw t0, %s\n" indent (riscv_of_place offset_table arg_pl);
              Printf.fprintf oc "%smv a%d, t0\n" indent i;
            )
            else
              failwith "todo: store in stack"
        ) args;

        Printf.fprintf oc "%sjal ra, %s\n" indent name;

        (* Si c'etait une procedure on a rien a mettre dans a0 *)
        let ret_type = typ_of_place prog mir_body retplace in
        if ret_type <> Tunit then(
          Printf.fprintf oc "%ssw a0, %s\n" indent (riscv_of_place offset_table retplace);
        );
        goto_next fundef oc curr_lbl next_lbl;

    | Igoto (dest_lbl) -> 
        Printf.fprintf oc "%sj %s\n" indent (label_name fundef dest_lbl)
    | Iif (place_to_check, then_lbl, else_lbl) -> 
      (* Si la place n'est pas egale a 0, elle est true, et on jump au then. Sinon, on jump au else*)
        Printf.fprintf oc "%slw t0, %s\n" indent (riscv_of_place offset_table place_to_check);
        Printf.fprintf oc "%sbne t0, zero, %s\n" indent (label_name fundef then_lbl);
        Printf.fprintf oc "%sj %s\n" indent (label_name fundef else_lbl);
        
        
    | Ireturn ->
        let ret_type = Hashtbl.find mir_body.mlocals Lret in
        if ret_type = Tunit then
          Printf.fprintf oc "%s# rien pour _ret = ()\n" indent
        else
          Printf.fprintf oc "%slw a0, %s\n" indent (riscv_of_place offset_table (PlLocal Lret));
        ;

        Printf.fprintf oc "%slw ra, %d(sp)\n" indent (frame_size - 4); (* On recupere le contexte dans ra *)
        Printf.fprintf oc "%saddi sp, sp, %d\n" indent frame_size; (* On rend la stack! *)
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

