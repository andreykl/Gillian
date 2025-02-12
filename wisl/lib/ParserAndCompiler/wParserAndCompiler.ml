open Lexing
open WLexer

type init_data = unit
type err = unit
type tl_ast = WProg.t

module Annot = WAnnot

let pp_err _ () = ()

let parse_with_error token lexbuf =
  try token read lexbuf with
  | SyntaxError message -> failwith ("SYNTAX ERROR" ^ message)
  | WParser.Error ->
      let range = CodeLoc.curr lexbuf in
      let message =
        Printf.sprintf "unexpected token : %s at loc %s" (Lexing.lexeme lexbuf)
          (CodeLoc.str range)
      in
      failwith ("PARSER ERROR : " ^ message)

let parse_file file =
  let inx = open_in file in
  let lexbuf = Lexing.from_channel inx in
  let () = lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = file } in
  let wprog = parse_with_error WParser.prog lexbuf in
  let () = close_in inx in
  wprog

let compile = Wisl2Gil.compile

let create_compilation_result path prog wprog =
  let open Command_line.ParserAndCompiler in
  let open IncrementalAnalysis in
  let source_files = SourceFiles.make () in
  let () = SourceFiles.add_source_file source_files ~path in
  let gil_path = Filename.chop_extension path ^ ".gil" in
  {
    gil_progs = [ (gil_path, prog) ];
    source_files;
    tl_ast = wprog;
    init_data = ();
  }

let parse_and_compile_files files =
  let f files =
    let path = List.hd files in
    let wprog = parse_file path in
    let prog_t = compile ~filepath:path wprog in
    let progs = create_compilation_result path prog_t wprog in
    let r = Ok progs in
    let pp_annot fmt annot =
      Fmt.pf fmt "%a"
        (Yojson.Safe.pretty_print ?std:None)
        (Annot.to_yojson annot)
    in
    let open Utils.Command_line_utils in
    let _ =
      burn_gil ~init_data:`Null
        ~pp_prog:(Gil_syntax.Prog.pp_labeled ~pp_annot)
        prog_t (Some "/home/andrey/tmp.gil")
    in
    (* let open Batteries in *)
    (* let _ = Printf.printf "-----------------%s\n" (dump r) in *)
    r
  in
  Logging.Phase.with_normal ~title:"Program parsing and compilation" (fun () ->
      f files)

let other_imports = []
let initialize _ = ()
let default_import_paths = Some Runtime_sites.Sites.runtime

module TargetLangOptions =
  Gillian.Command_line.ParserAndCompiler.Dummy.TargetLangOptions
