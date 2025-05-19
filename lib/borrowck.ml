(* Once you are done writing the code, remove this directive,
   whose purpose is to disable several warnings. *)
[@@@warning "-26-27"]

open Type
open Minimir
open Active_borrows

(* This function computes the set of alive lifetimes at every program point. *)
let compute_lft_sets prog mir : lifetime -> PpSet.t =

  (* The [outlives] variable contains all the outlives relations between the
    lifetime variables of the function. *)
  let outlives = ref LMap.empty in

  (* Helper functions to add outlives constraints. *)
  let add_outlives (l1, l2) = outlives := add_outlives_edge l1 l2 !outlives in
  let unify_lft l1 l2 =
    add_outlives (l1, l2);
    add_outlives (l2, l1)
  in

  (* First, we add in [outlives] the constraints implied by the type of locals. *)
  Hashtbl.iter
    (fun _ typ -> outlives := outlives_union !outlives (implied_outlives prog typ))
    mir.mlocals;

  (* Then, we add the outlives relations needed for the instructions to be safe. *)

  (* TODO: generate these constraints by
       - unifying types that need be equal (note that MiniRust does not support subtyping, that is,
         if a variable x: &'a i32 is used as type &'b i32, then this requires that lifetimes 'a and
         'b are equal),
       - adding constraints required by function calls,
       - generating constraints corresponding to reborrows. More precisely, if we create a borrow
         of a place that dereferences  borrows, then the lifetime of the borrow we
         create should be shorter than the lifetimes of the borrows the place dereference.
         For example, if x: &'a &'b i32, and we create a borrow y = &**x of type &'c i32,
         then 'c should be shorter than 'a and 'b.

    SUGGESTION: use functions [typ_of_place], [fields_types_fresh] and [fn_prototype_fresh].
  *)


  let rec collect_borrow_lifetimes_aux pl acc = 
    match pl with
    | PlDeref borrowed_place ->(
        match typ_of_place prog mir borrowed_place with
        | Tborrow (borrow_lifetime, _, _) -> collect_borrow_lifetimes_aux borrowed_place (LSet.add borrow_lifetime acc)
        | _ -> failwith "(borrowck) Erreur: dereferencement d'un non-emprunt"
    )
    | PlField (pl, _) -> collect_borrow_lifetimes_aux pl acc
    | PlLocal _  -> acc
  in
    
  let collect_borrow_lifetimes pl = 
    collect_borrow_lifetimes_aux pl (LSet.empty)
  in

  let rec unify_types typ1 typ2 = 
  match typ1, typ2 with
  | Tstruct (name1, elem_lifetimes1), Tstruct (name2, elem_lifetimes2) ->
      if name1 = name2 then
        List.iter2 (fun lifetime1 lifetime2 -> unify_lft lifetime1 lifetime2) elem_lifetimes1 elem_lifetimes2
      else
        failwith "(borrowck) Structs impossible a unifier. (devrait etre unreachable)"
  | Tborrow (lft1, mut1, typ1), Tborrow (lft2, mut2, typ2) -> 
      if mut1 = mut2 then(
        unify_types typ1 typ2;
        unify_lft lft1 lft2;
      )
      else
        failwith "(borrowck) Borrows impossible a unifier. (devrait etre unreachable)"
  | Tunit, Tunit | Ti32, Ti32 | Tbool, Tbool -> () (* pas de durées de vie a unifier.. *)
  | _, _ -> 
        failwith "(borrowck) Types incompatibles impossible a unifier. (devrait etre unreachable)"
  in

  Array.iteri
  (fun lbl (instr, loc) ->
    match instr with
    | Iassign (place_dest, rv, this_lbl) ->(
        match rv with
        | RVplace place_src -> 
            let typ_dest = typ_of_place prog mir place_dest in
            let typ_src = typ_of_place prog mir place_src in
            unify_types typ_dest typ_src
        | RVborrow (_, borrowed_place) -> (
            match typ_of_place prog mir place_dest with
            | Tborrow (dest_lft, _, _) -> 
                LSet.iter (fun curr_lft -> add_outlives (dest_lft, curr_lft) ) (collect_borrow_lifetimes borrowed_place)
            | _ -> failwith "(borrowck) RVborrow n'a pas le type Tborrow (unreachable)"
        )
        | RVmake (struct_name, struct_fields) -> 
            (
            match fields_types_fresh prog struct_name with
            | proto_fields_types, proto_struct_type -> 
                let typ_dest = typ_of_place prog mir place_dest in
                unify_types typ_dest proto_struct_type;
                List.iter2 (
                  fun struct_field proto_field_type ->
                    let struct_field_type = typ_of_place prog mir struct_field in
                    unify_types struct_field_type proto_field_type
                ) struct_fields proto_fields_types
            )
        | RVunop _ | RVbinop _| RVconst _ | RVunit -> ()
        ) 
    | Icall (name, args, retplace, func_lbl) -> 
        (
        match fn_prototype_fresh prog name with
        | proto_arg_types, proto_return_type, func_constraints -> 
            List.iter ( fun curr_c -> add_outlives curr_c ) func_constraints;
            
            let ret_typ = typ_of_place prog mir retplace in
            unify_types ret_typ proto_return_type;

            List.iter2 (
              fun arg prototype_arg_type -> 
                let arg_type = typ_of_place prog mir arg in
                unify_types arg_type prototype_arg_type
            ) args proto_arg_types;
        )
    | Ideinit _ | Igoto _ | Iif _ | Ireturn -> ()
    )
  mir.minstrs;

  (* The [living] variable contains constraints of the form "lifetime 'a should be
    alive at program point p". *)
  let living : PpSet.t LMap.t ref = ref LMap.empty in

  (* Helper function to add living constraint. *)
  let add_living pp l =
    living :=
      LMap.update l
        (fun s -> Some (PpSet.add pp (Option.value s ~default:PpSet.empty)))
        !living
  in

  (* Run the live local analysis. See module Live_locals for documentation. *)
  let live_locals = Live_locals.go mir in

  (* TODO (DONE): generate living constraints:
     - Add living constraints corresponding to the fact that liftimes appearing free
       in the type of live locals at some program point should be alive at that
       program point.
     - Add living constraints corresponding to the fact that generic lifetime variables
       (those in [mir.mgeneric_lfts]) should be alive during the whole execution of the
       function.
  *)

  (* If [lft] is a generic lifetime, [lft] is always alive at [PpInCaller lft]. *)
  List.iter (fun lft -> add_living (PpInCaller lft) lft) mir.mgeneric_lfts;

  (* Now, we compute lifetime sets by finding the smallest solution of the constraints, using the
    Fix library. *)
  let module Fix = Fix.Fix.ForType (struct type t = lifetime end) (Fix.Prop.Set (PpSet))
  in
  Fix.lfp (fun lft lft_sets ->
      LSet.fold
        (fun lft acc -> PpSet.union (lft_sets lft) acc)
        (Option.value ~default:LSet.empty (LMap.find_opt lft !outlives))
        (Option.value ~default:PpSet.empty (LMap.find_opt lft !living)))

let borrowck prog mir =
  (* We check initializedness requirements for every instruction. *)
  let uninitialized_places = Uninitialized_places.go prog mir in
  Array.iteri
    (fun lbl (instr, loc) ->
      let uninit : PlaceSet.t = uninitialized_places lbl in

      let check_initialized pl =
        if PlaceSet.exists (fun pluninit -> is_subplace pluninit pl) uninit then
          Error.error loc "Use of a place which is not fully initialized at this point."
      in

      (match instr with
      | Iassign (pl, _, _) | Icall (_, _, pl, _) -> (
          match pl with
          | PlDeref pl0 ->
              if PlaceSet.mem pl0 uninit then
                Error.error loc "Writing into an uninitialized borrow."
          | PlField (pl0, _) ->
              if PlaceSet.mem pl0 uninit then
                Error.error loc "Writing into a field of an uninitialized struct."
          | _ -> ())
      | _ -> ());

      match instr with
      | Iassign (_, RVplace pl, _) | Iassign (_, RVborrow (_, pl), _) ->
          check_initialized pl
      | Iassign (_, RVbinop (_, pl1, pl2), _) ->
          check_initialized pl1;
          check_initialized pl2
      | Iassign (_, RVunop (_, pl), _) | Iif (pl, _, _) -> check_initialized pl
      | Iassign (_, RVmake (_, pls), _) | Icall (_, pls, _, _) ->
          List.iter check_initialized pls
      | Ireturn -> check_initialized (PlLocal Lret)
      | Iassign (_, (RVunit | RVconst _), _) | Ideinit _ | Igoto _ -> ())
    mir.minstrs;

  (* We check the code honors the non-mutability of shared borrows. *)
  Array.iteri
    (fun _ (instr, loc) ->
      (* TODO (DONE): check that we never write to shared borrows, and that we never create mutable borrows
        below shared borrows. Function [place_mut] can be used to determine if a place is mutable, i.e., if it
        does not dereference a shared borrow. *)
        match instr with
        | Iassign (_, RVborrow (Mut, borrowed_place), _) ->
            let mutability = place_mut prog mir borrowed_place in
            if mutability = NotMut then
              Error.error loc "Cannot create a mutable borrow of a shared borrow"
            else
              ()
        | Icall (_, _, place_dest, _) | Iassign (place_dest, _, _) -> 
            let mutability = place_mut prog mir place_dest in
            if mutability = NotMut then
              Error.error loc "Tried to write a value to a shared borrow" (* Si je comprends bien, les places immutables sont toujours dans des shared borrows *)
            else
              ()
            
        | _ -> ()
    )
    mir.minstrs;

  let lft_sets = compute_lft_sets prog mir in

  (* TODO: check that outlives constraints declared in the prototype of the function are
    enough to ensure safety. I.e., if [lft_sets lft] contains program point [PpInCaller lft'], this
    means that we need that [lft] be alive when [lft'] dies, i.e., [lft'] outlives [lft]. This relation
    has to be declared in [mir.outlives_graph]. *)
  List.iter (
    fun lft -> 
      let valid_points = lft_sets lft in
      PpSet.iter(
        fun point ->
          match point with
          | PpInCaller lft' -> (
              match LMap.find_opt lft' mir.moutlives_graph with
              | None ->
                  Error.error mir.mloc "Missing outlives constraint"
              | Some lifetime_set ->
                  if not (LSet.mem lft lifetime_set) then
                    Error.error mir.mloc "Missing outlives constraint"
              )
          | _ -> ()
      ) valid_points
  ) mir.mgeneric_lfts;

  (* We check that we never perform any operation which would conflict with an existing
    borrows. *)
  let bor_active_at = Active_borrows.go prog lft_sets mir in
  Array.iteri
    (fun lbl (instr, loc) ->
      (* The list of bor_info for borrows active at this instruction. *)
      let active_borrows_info : bor_info list =
        List.map (get_bor_info prog mir) (BSet.to_list (bor_active_at lbl))
      in

      (* Does there exist a borrow of a place pl', which is active at program point [lbl],
        such that a *write* to [pl] conflicts with this borrow?

         If [pl] is a subplace of pl', then writing to [pl] is always conflicting, because
        it is aliasing with the borrow of pl'.

         If pl' is a subplace of [pl], the situation is more complex:
           - if pl' involves as many dereferences as [pl] (e.g., writing to [x.f1] while
            [x.f1.f2] is borrowed), then the write to [pl] will overwrite pl', hence this is
            conflicting.
           - BUT, if pl' involves more dereferences than [pl] (e.g., writing to [x.f1] while
            [*x.f1.f2] is borrowed), then writing to [pl] will *not* modify values accessible
            from pl'. Hence, subtlely, this is not a conflict. *)
      let conflicting_borrow_no_deref pl =
        List.exists
          (fun bi -> is_subplace pl bi.bplace || is_subplace_no_deref bi.bplace pl)
          active_borrows_info
      in

      (match instr with
      | Iassign (pl, _, _) | Icall (_, _, pl, _) ->
          if conflicting_borrow_no_deref pl then
            Error.error loc "Assigning a borrowed place."
      | Ideinit (l, _) ->
          if conflicting_borrow_no_deref (PlLocal l) then
            Error.error loc
              "A local declared here leaves its scope while still being borrowed."
      | Ireturn ->
          Hashtbl.iter
            (fun l _ ->
              match l with
              | Lparam p ->
                  if conflicting_borrow_no_deref (PlLocal l) then
                    Error.error loc
                      "When returning from this function, parameter `%s` is still \
                       borrowed."
                      p
              | _ -> ())
            mir.mlocals
      | _ -> ());

      (* Variant of [conflicting_borrow_no_deref]: does there exist a borrow of a place pl',
        which is active at program point [lbl], such that a *read* to [pl] conflicts with this
        borrow? In addition, if parameter [write] is true, we consider an operation which is
        both a read and a write. *)
      let conflicting_borrow write pl =
        List.exists
          (fun bi ->
            (bi.bmut = Mut || write)
            && (is_subplace pl bi.bplace || is_subplace bi.bplace pl))
          active_borrows_info
      in

      (* Check a "use" (copy or move) of place [pl]. *)
      let check_use pl =
        let consumes = not (typ_is_copy prog (typ_of_place prog mir pl)) in
        if conflicting_borrow consumes pl then
          Error.error loc "A borrow conflicts with the use of this place.";
        if consumes && contains_deref_borrow pl then
          Error.error loc "Moving a value out of a borrow."
      in

      match instr with
      | Iassign (_, RVunop (_, pl), _) -> check_use pl
      | Iassign (_, RVborrow (mut, pl), _) ->
          if conflicting_borrow (mut = Mut) pl then
            Error.error loc "There is a borrow conflicting this borrow."
      | _ -> () (* TODO: complete the other cases*)
    )
    mir.minstrs
