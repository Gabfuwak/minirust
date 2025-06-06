open Minirust
open Ast

let () =
  if Array.length Sys.argv < 2 then Error.usage ();
  let filename = Sys.argv.(1) in
  let output_file = if Array.length Sys.argv >= 3 then Some Sys.argv.(2) else None in
  try
    let prog = Parser_entry.parse_file filename in

    Typecheck.go prog;

    Hashtbl.iter
      (fun _ d ->
        match d with
        | Dfundef fd ->
            let mir = Emit_minimir.emit_fun prog fd in

            if Sys.getenv_opt "MINIMIR" <> None then (
              Printf.printf "=== %s ===\n" fd.fname.id;
              Print_minimir.print mir;
              Printf.printf "\n");

            Borrowck.borrowck prog mir
        | _ -> ())
      prog;

    (match output_file with
    | Some out ->
        if Sys.getenv_opt "RISCV" <> None then Emit_riscv.emit_riskv prog out
        else Emit_c.emit_c prog out
    | None ->
        if Sys.getenv_opt "RISCV" <> None then Emit_riscv.emit_riskv prog "/dev/stdout"
        else ());

    ()
  with Error.Error (start, end_, msg) ->
    Printf.eprintf "%s" (MenhirLib.LexerUtil.range (start, end_));
    Printf.eprintf "%s\n" msg;
    exit 1
