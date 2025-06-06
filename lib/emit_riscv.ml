open Minimir
open Ast

let indent = "  "

type riscv_location = 
  | ComputedStack of int  (* offset par rapport à sp *)
  | ComputedReg of string * int (* nécessite génération de code, un peu dirty faute de meilleure idée *)

let riscv_of_location loc =
  match loc with
  | ComputedStack offset -> Printf.sprintf "%d(sp)" offset
  | ComputedReg (reg, 0) -> reg (* Cas le plus simple, rien a aller chercher dans la stack *)
  | ComputedReg (reg, offset) -> Printf.sprintf "%d(%s)" offset reg

let label_name fundef lbl = 
  Printf.sprintf "%s_L%d" fundef.fname.id lbl

let string_of_instr instr =
  Format.asprintf "%a" Print_minimir.pp_instr instr

let rec size_of_typ prog typ =
  match typ with
  | Tbool -> 1
  | Tunit -> 0
  | Ti32 -> 4
  | Tstruct (name, _) ->
      let struct_def = get_struct_def prog name in
      List.fold_left (fun acc (_, field_typ) ->
        acc + size_of_typ  prog field_typ
      ) 0 struct_def.sfields
  | Tborrow _ -> 4

let field_offset prog struct_name field_name =
  let struct_def = get_struct_def prog struct_name in
  let rec calc_offset acc = function
    | [] -> failwith "field not found"
    | (fname, _) :: _ when fname.id = field_name -> acc
    | (_, ftyp) :: rest -> calc_offset (acc + size_of_typ prog ftyp) rest
  in calc_offset 0 struct_def.sfields

let compute_all_offsets prog fundef mir_body =
  let offset_table = Hashtbl.create 16 in
  let current_offset = ref 0 in
  
  Hashtbl.iter (fun local typ ->
    match local with
    | Lvar id ->
        let size = size_of_typ prog typ in
        let aligned_size = ((size + 3) / 4) * 4 in
        current_offset := !current_offset + aligned_size;
        Hashtbl.add offset_table id (!current_offset)
    | Lparam name ->  (* a0-a7, TODO: ajouter le calcul correct pour generer le cas avec plus de 8 paramètres *)
        let param_idx = (
          match List.find_index (fun (id, _, _) -> id.id = name) fundef.fformals with
          | Some idx -> idx
          | None  -> failwith "unreachable"
        )
        in

        if param_idx < 8 then
        ()
        else
          let size = size_of_typ prog typ in
          let aligned_size = ((size + 3) / 4) * 4 in
          current_offset := !current_offset + aligned_size;
          Hashtbl.add offset_table (-param_idx) (!current_offset) (* on met -idx, meme principe que pour Lret et en plus c'est simple a récuperer *)

    | Lret ->
        let size = size_of_typ prog typ in
        let aligned_size = ((size + 3) / 4) * 4 in
        current_offset := !current_offset + aligned_size;
        Hashtbl.add offset_table (-1) (!current_offset) (* on met -1 comme id special pour la valeur de retour, on est sur que ça ne sera jamais pris *)
  ) mir_body.mlocals;
  
  (!current_offset, offset_table)

let rec location_of_place mir_body prog fundef offset_table pl =
  match pl with
  | PlLocal (Lvar id) -> 
      let offset = Hashtbl.find offset_table id in
      ComputedStack (-offset)
  | PlLocal (Lparam name) -> 
      let param_idx = (
        match List.find_index (fun (id, _, _) -> id.id = name) fundef.fformals with
        | Some idx -> idx
        | None  -> failwith "unreachable"
      )
      in

      if param_idx < 8 then
        ComputedReg (Printf.sprintf "a%d" param_idx, 0)
      else
        let offset = Hashtbl.find offset_table (-param_idx) in
        ComputedStack (-offset)

  | PlLocal Lret -> 
      let offset = Hashtbl.find offset_table (-1) in
      ComputedStack (-offset)
  | PlDeref _ -> failwith "todo riscv_of_place deref"
  | PlField (base_place, field_name) -> 
      let base_loc = location_of_place mir_body prog fundef offset_table base_place in
      let struct_name = match typ_of_place prog mir_body base_place with
        | Tstruct (name, _) -> name
        | _ -> failwith "PlField on non-struct type (unreachable, caught by typechecker)"
      in
      let field_offset_val = field_offset prog struct_name field_name in
      (match base_loc with
       | ComputedStack base_offset -> 
           ComputedStack (base_offset + field_offset_val)
       | ComputedReg (reg, base_offset)-> 
          ComputedReg (reg, base_offset + field_offset_val)
      )

let riscv_of_rvalue prog mir_body fundef offset_table rval =
  match rval with
  | RVunit -> "" 
  | RVconst (Cbool true) -> "1"
  | RVconst (Cbool false) -> "0"
  | RVconst (Ci32 a) ->
      (* On enleve le suffixe i32 du litteral *)
      let len = String.length a in
      String.sub a 0 (len - 3)
  | RVplace pl ->
      riscv_of_location (location_of_place mir_body prog fundef offset_table pl)
  | _ -> failwith "riscv_of_rvalue: complex operation, handle in Iassign"

let goto_next fundef oc curr_lbl next_lbl =
  if next_lbl = curr_lbl + 1 then
    () (* No-op, c'est séquentiel *)
  else
    (* Avec l'implementation actuelle de minimir je crois que ça devrait etre unreachable mais autant etre robuste *)
    Printf.fprintf oc "%sj %s\n" indent (label_name fundef next_lbl) 

let rec emit_assignment oc src_loc dst_loc =
  (match src_loc, dst_loc with
   | ComputedReg (src_reg, 0), ComputedReg (dst_reg, 0) ->
       Printf.fprintf oc "%smv %s, %s\n" indent dst_reg src_reg
   | ComputedReg (src_reg, 0), ComputedStack _ ->
       Printf.fprintf oc "%ssw %s, %s\n" indent src_reg (riscv_of_location dst_loc)
   | ComputedReg (src_reg, offset), _ when offset != 0 ->
       Printf.fprintf oc "%slw t0, %d(%s)\n" indent offset src_reg;
       (* Puis traiter t0 → dst_loc *)
       emit_assignment oc (ComputedReg("t0", 0)) dst_loc
   | ComputedStack _, ComputedReg (dreg, 0) ->
       Printf.fprintf oc "%slw %s, %s\n" indent dreg (riscv_of_location src_loc)
   | ComputedStack _, ComputedStack _ ->
       Printf.fprintf oc "%slw t0, %s\n" indent (riscv_of_location src_loc);
       Printf.fprintf oc "%ssw t0, %s\n" indent (riscv_of_location dst_loc)
   | _ -> failwith "emit_assignment: unsupported case"
  )

let emit_function prog fundef mir_body oc = 

  let (_curr_offset, offset_table) = compute_all_offsets prog fundef mir_body in
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
    | Iassign (pl_dest, rval, next_lbl) -> 
    (match rval with
     | RVunit ->
         (* No-op pour unit *)
         ()
     | RVplace src_place ->
         (* Place → place : gestion registres/stack *)
         let src_loc = location_of_place mir_body prog fundef offset_table src_place in
         let dst_loc = location_of_place mir_body prog fundef offset_table pl_dest in
         emit_assignment oc src_loc dst_loc
     | RVconst _ ->
         Printf.fprintf oc "%sli t0, %s\n" indent (riscv_of_rvalue prog mir_body fundef offset_table rval);
         let dst_loc = location_of_place mir_body prog fundef offset_table pl_dest in
         (match dst_loc with
          | ComputedReg _ -> Printf.fprintf oc "%smv %s, t0\n" indent (riscv_of_location dst_loc)
          | ComputedStack _ -> Printf.fprintf oc "%ssw t0, %s\n" indent (riscv_of_location dst_loc)
         )
     | RVunop (op, place) ->
         let src_loc = location_of_place mir_body prog fundef offset_table place in
         let dst_loc = location_of_place mir_body prog fundef offset_table pl_dest in
         
          (* Pas la meme operation selon le type *)
         (match src_loc with
          | ComputedReg _ -> Printf.fprintf oc "%smv t0, %s\n" indent (riscv_of_location src_loc)
          | ComputedStack _ -> Printf.fprintf oc "%slw t0, %s\n" indent (riscv_of_location src_loc)
         );
         
         (* Opération *)
         (match op with
          | Uneg -> Printf.fprintf oc "%ssub t0, zero, t0\n" indent
          | Unot -> Printf.fprintf oc "%sxori t0, t0, 1\n" indent
         );
         
         (match dst_loc with
          | ComputedReg _ -> Printf.fprintf oc "%smv %s, t0\n" indent (riscv_of_location dst_loc)
          | ComputedStack _ -> Printf.fprintf oc "%ssw t0, %s\n" indent (riscv_of_location dst_loc)
         );

     | RVbinop (op, place1, place2) -> 
         let src_loc1 = location_of_place mir_body prog fundef offset_table place1 in
         let src_loc2 = location_of_place mir_body prog fundef offset_table place2 in
         let dst_loc = location_of_place mir_body prog fundef offset_table pl_dest in
         (match src_loc1 with
          | ComputedReg _ -> Printf.fprintf oc "%smv t0, %s\n" indent (riscv_of_location src_loc1)
          | ComputedStack _ -> Printf.fprintf oc "%slw t0, %s\n" indent (riscv_of_location src_loc1)
         );

        (match src_loc2 with
          | ComputedReg _ -> Printf.fprintf oc "%smv t1, %s\n" indent (riscv_of_location src_loc2)
          | ComputedStack _ -> Printf.fprintf oc "%slw t1, %s\n" indent (riscv_of_location src_loc2)
          );

        (match op with
         | Badd -> Printf.fprintf oc "%sadd t0, t0, t1\n" indent
         | Bsub -> Printf.fprintf oc "%ssub t0, t0, t1\n" indent
         | Bmul -> Printf.fprintf oc "%smul t0, t0, t1\n" indent
         | Bdiv -> Printf.fprintf oc "%sdiv t0, t0, t1\n" indent
         | Bmod -> Printf.fprintf oc "%srem t0, t0, t1\n" indent
         | Beqeq ->
            Printf.fprintf oc "%sxor t0, t0, t1\n" indent;
            Printf.fprintf oc "%sseqz t0, t0\n" indent
         | Bneq -> 
            Printf.fprintf oc "%sxor t0, t0, t1\n" indent;
            Printf.fprintf oc "%ssnez t0, t0\n" indent
         | Blt -> Printf.fprintf oc "%sslt t0, t0, t1\n" indent
         | Bge -> 
            Printf.fprintf oc "%sslt t0, t0, t1\n" indent;
            Printf.fprintf oc "%sxori t0, t0, 1\n" indent
         | Bgt -> Printf.fprintf oc "%sslt t0, t1, t0\n" indent
         | Ble -> 
            Printf.fprintf oc "%sslt t0, t1, t0\n" indent;
            Printf.fprintf oc "%sxori t0, t0, 1\n" indent
        );

        (match dst_loc with
          | ComputedReg _ -> Printf.fprintf oc "%smv %s, t0\n" indent (riscv_of_location dst_loc)
          | ComputedStack _ -> Printf.fprintf oc "%ssw t0, %s\n" indent (riscv_of_location dst_loc)
         );

          ()
     | RVmake (struct_name, args) ->
          let struct_def = get_struct_def prog struct_name in
          List.iter2 (fun (field_id, _) src_place ->
            let src_loc = location_of_place mir_body prog fundef offset_table src_place in
            let dst_loc = location_of_place mir_body prog fundef offset_table (PlField(pl_dest, field_id.id)) in
            emit_assignment oc src_loc dst_loc
          ) struct_def.sfields args
     | _ -> failwith "todo: implement case for rvalue assign"
    );
    goto_next fundef oc curr_lbl next_lbl

    | Icall (name, args, retplace, next_lbl) ->
        List.iteri (
          fun i arg_pl ->
            if i < 8 then(
              Printf.fprintf oc "%slw t0, %s\n" indent (riscv_of_location (location_of_place mir_body prog fundef offset_table arg_pl));
              Printf.fprintf oc "%smv a%d, t0\n" indent i;
            )
            else
              failwith "todo: store in stack"
        ) args;

        Printf.fprintf oc "%sjal ra, %s\n" indent name;

        (* Si c'etait une procedure on a rien a mettre dans a0 *)
        let ret_type = typ_of_place prog mir_body retplace in
        if ret_type <> Tunit then(
          Printf.fprintf oc "%ssw a0, %s\n" indent (riscv_of_location (location_of_place mir_body prog fundef offset_table retplace));
        );
        goto_next fundef oc curr_lbl next_lbl;

    | Igoto (dest_lbl) -> 
        Printf.fprintf oc "%sj %s\n" indent (label_name fundef dest_lbl)
    | Iif (place_to_check, then_lbl, else_lbl) -> 
      (* Si la place n'est pas egale a 0, elle est true, et on jump au then. Sinon, on jump au else*)
        Printf.fprintf oc "%slw t0, %s\n" indent (riscv_of_location (location_of_place mir_body prog fundef offset_table place_to_check));
        Printf.fprintf oc "%sbne t0, zero, %s\n" indent (label_name fundef then_lbl);
        Printf.fprintf oc "%sj %s\n" indent (label_name fundef else_lbl);
        
        
    | Ireturn ->
        let ret_type = Hashtbl.find mir_body.mlocals Lret in
        if ret_type = Tunit then
          Printf.fprintf oc "%s# rien pour _ret = ()\n" indent
        else
          Printf.fprintf oc "%slw a0, %s\n" indent (riscv_of_location (location_of_place mir_body prog fundef offset_table (PlLocal Lret)));
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
    | Dstruct _ -> ()
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

