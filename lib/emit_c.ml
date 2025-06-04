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

let extract_type_deps typ =
  match typ with
  | Tstruct (name, _) -> [name]
  | Tborrow _ -> []  (* On peut faire de la dependence circulaire avec les type incomplets, dans ce cas la on ne crée juste pas de dependance dans notre graphe *)
  | Tunit | Ti32 | Tbool -> []

let extract_decl_deps decl =
  match decl with
  | Dstruct s ->
      List.fold_left (fun acc (_, field_type) ->
        acc @ extract_type_deps field_type
      ) [] s.sfields
  | Dfundef _ -> []
  
let build_dependency_graph prog =
  let add_dependencies graph decl_name deps =
    let existing = 
      match Hashtbl.find_opt graph decl_name with
      | Some lst -> lst
      | None -> []
    in
    Hashtbl.replace graph decl_name (existing @ deps)
  in

  let graph = Hashtbl.create 16 in
  (* On initialise toutes les declaration *)
  Hashtbl.iter (fun _ decl ->
    match decl with
    | Dstruct s -> 
        if not (Hashtbl.mem graph s.sname.id) then
          Hashtbl.add graph s.sname.id []
    | _ -> () (* Pour l'instant on a besoin que des structs, mais dans le futur on pourra ajouter les fonctions ici si besoin pour de l'optimisation ou pour le faire en assembleur *)
  ) prog;

  (* On ajoute les relations de dependance *)
  Hashtbl.iter (fun _ decl ->
    match decl with
    | Dstruct s ->
        let deps = extract_decl_deps decl in
        add_dependencies graph s.sname.id deps
    | _ -> () (* meme chose que juste avant *)
  ) prog;

  graph
    

let topological_sort graph =
  (* Algorithme de khan *)
  (* possible todo: on pourrait ameliorer la lisibilité du code output en suivant les branches 
     pour par exemple que les composantes connexes soient a la suite, ou que les graphes qui ressemblent 
     a des linked lists soient a la suite dans l'output *)

  (* On compte le nombre d'aretes entrantes *)
  let in_degrees = Hashtbl.create (Hashtbl.length graph) in

  Hashtbl.iter (
    fun node _ -> 
      Hashtbl.add in_degrees node 0
  ) graph;

  Hashtbl.iter(
    fun _ out_edges ->
      List.iter (
        fun target ->
          let current = 
            match Hashtbl.find_opt in_degrees target with
            | Some n -> n
            | None -> 0
          in
          Hashtbl.replace in_degrees target (current + 1)
      ) out_edges

  ) graph;
  
  (* On initialise les degrés a 0 *)
  let queue = Queue.create () in
  Hashtbl.iter (fun node degree ->
    if degree = 0 then
      Queue.add node queue
  ) in_degrees;

  let result = ref [] in
  while not (Queue.is_empty queue) do
    let node = Queue.take queue in
    result := node :: !result;
    

    (* On reduit de 1 le degre des noeuds dependants *)
    let deps = Hashtbl.find graph node in
    List.iter (fun dep ->
      let new_degree = (Hashtbl.find in_degrees dep) - 1 in
      Hashtbl.replace in_degrees dep new_degree;
      if new_degree = 0 then
        Queue.add dep queue
    ) deps
  done;

  if List.length !result <> Hashtbl.length graph then
    failwith "emit_c.ml: Dependence circulaire trouvée! (devrait etre unreachable)" (* Normalement le typechecker gère déjà ça*)
  else
    !result


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
  
  Printf.fprintf oc ");\n";
  ()

  
let emit_struct_typedef struct_decl oc =
  Printf.fprintf oc "typedef struct {\n";
  List.iter (
    fun (field_name,field_type) ->
      Printf.fprintf oc "    %s %s;\n" (ctyp_of_mirtyp field_type) field_name.id
  ) struct_decl.sfields;

  Printf.fprintf oc "} %s;\n\n" struct_decl.sname.id

let c_of_rvalue rval =
  failwith "todo"

let c_of_place pl =
  failwith "todo"

let indent_of_scope scope = 
  String.make (scope * 4) ' '

let emit_function_impl prog fundef mir_body oc = 
  emit_function_header fundef.fname.id mir_body oc; (* On emit le header sans le ; pour s'eviter de la duplication de code*)
  let scope = ref 0 in
  Printf.fprintf oc "{\n";
  scope := !scope + 1;

  (* emit definitions de locales *)
  Hashtbl.iter (
    fun local typ ->
      match local with
      | Lvar varid -> 
        Printf.fprintf oc "%s%s %s;\n" (indent_of_scope !scope) (ctyp_of_mirtyp typ) (Printf.sprintf "_localvar%d" varid)
      | Lret -> 
        if typ <> Tunit then
          Printf.fprintf oc "%s%s _ret;" (indent_of_scope !scope) (ctyp_of_mirtyp typ)
      | _ -> ()
  ) mir_body.mlocals;

  Array.iteri (
    fun curr_lbl (instr, _)->
    (* Emit le label (note: j'aime pas faire ça comme ça, c'est de l'assembleur with extra steps, si ce commentaire reste dans le code final ma deception est immense et ma peine est inconsolable) *)
    Printf.fprintf oc "L%d:\n" curr_lbl;
    match instr with
    | Iassign (pl_dest, rval, next_lbl) -> 
        Printf.fprintf oc "%s%s = %s;\n" (indent_of_scope !scope) (c_of_place pl_dest) (c_of_rvalue rval);
        Printf.fprintf oc "%sgoto L%d;\n" (indent_of_scope !scope) next_lbl
    | Icall (name, args, retplace, next_lbl) ->
        Printf.fprintf oc "%s" (indent_of_scope !scope);
        let ret_type = typ_of_place prog mir_body retplace in
        if ret_type <> Tunit then
          Printf.fprintf oc "%s = " (c_of_place retplace) 
        ;
        Printf.fprintf oc "%s(" name;
        List.iteri (
          fun i arg_pl ->
            if i < (List.length args) - 1 then
              Printf.fprintf oc "%s, " (c_of_place arg_pl)
            else
              Printf.fprintf oc "%s" (c_of_place arg_pl)
        ) args;
        Printf.fprintf oc ");\n";

        Printf.fprintf oc "%sgoto L%d;\n" (indent_of_scope !scope) next_lbl

    | Igoto (dest_lbl) -> 
        Printf.fprintf oc "%sgoto L%d;\n" (indent_of_scope !scope) dest_lbl
    | Iif (place_to_check, then_lbl, else_lbl) -> 
        (* un peu overkill, ça pourrait se faire en une ligne mais je pense que ce sera plus simple pour faire de l'idiomatique ensuite comme ça *)
        Printf.fprintf oc "%sif (%s) {\n" (indent_of_scope !scope) (c_of_place place_to_check);
        scope := !scope + 1;
        Printf.fprintf oc "%sgoto L%d;\n" (indent_of_scope !scope) then_lbl;
        scope := !scope - 1;
        Printf.fprintf  oc "%s} else {\n" (indent_of_scope !scope);
        scope := !scope + 1;
        Printf.fprintf oc "%sgoto L%d;\n" (indent_of_scope !scope) else_lbl;
        scope := !scope - 1;
        Printf.fprintf  oc "%s}\n" (indent_of_scope !scope);
        
        
    | Ireturn ->
        let ret_type = Hashtbl.find mir_body.mlocals Lret in
        if ret_type = Tunit then
          Printf.fprintf oc "%sreturn;\n" (indent_of_scope !scope)
        else
          Printf.fprintf oc "%sreturn _ret;\n" (indent_of_scope !scope)
    | Ideinit _ -> () (* rien a faire *)
  ) mir_body.minstrs;



  Printf.fprintf oc "}\n\n";
  ()
  
  


let emit_c prog output_file =
  let oc = open_out output_file in
  
  (* emit boilerplate *)
  Printf.fprintf oc "#include <stdint.h>\n";
  Printf.fprintf oc "#include <stdbool.h>\n\n";


  let dep_graph = build_dependency_graph prog in
  let struct_order = topological_sort dep_graph in
  
  (* Emit struct typedefs first*)
  List.iter (fun struct_name ->
    match Hashtbl.find_all prog struct_name with
    | [] -> ()
    | decls ->
        List.iter (fun decl ->
          match decl with
          | Dstruct d when d.sname.id = struct_name ->
              emit_struct_typedef d oc
          | _ -> ()
        ) decls
  ) struct_order;

  (* The emit functions prototypes after struct typedefs because we might need them *)
  (* We do prototypes first and not function implementations directly because i don't want to deal with function order *)
  let mir_bodies = Hashtbl.fold (fun _ decl acc ->
    match decl with
    | Dstruct _ -> acc
    | Dfundef fd ->
      let mir = Emit_minimir.emit_fun prog fd in 
      emit_function_header fd.fname.id mir oc;
      Printf.fprintf oc ";\n"; (* Je met ça la pour ne pas repeter de code dans l'emit de l'implementation de fonctions*)
    (fd, mir) :: acc
  ) prog [] in


  List.iter (
    fun (fundef, body) ->
      emit_function_impl prog fundef body oc
  ) mir_bodies;

  let _ = mir_bodies in (*todo, function implem*)

  


  (* TODO: emit bodies *)

  
  close_out oc

