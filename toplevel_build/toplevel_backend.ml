open Js_of_ocaml
open Js_of_ocaml_toplevel

module Ppx_support = struct
  let init () = Ast_mapper.register "js_of_ocaml" (fun _ -> Ppx_js.mapper)
end

let exec' s =
  let res : bool = JsooTop.use Format.std_formatter s in
  if not res then Format.eprintf "error while evaluating %s@." s

let setup_toplevel () =
  JsooTop.initialize ();
  Sys.interactive := false;
  if Version.comp Version.current [ 4; 07 ] >= 0 then exec' "open Stdlib";
  exec' "print_string (\"        OCaml version \" ^ Sys.ocaml_version);;";
  exec' "#enable \"pretty\";;";
  exec' "#disable \"shortvar\";;";
  exec' "#directory \"/static\";;";
  exec' "module Num = Big_int_Z;;";
  Ppx_support.init ();
  let[@alert "-deprecated"] new_directive n k = Hashtbl.add Toploop.directive_table n k in
    new_directive
    "load_js"
    (Toploop.Directive_string (fun name -> Js.Unsafe.global##load_script_ name));
  Sys.interactive := true;
  ()

let execute printval ?pp_code ?highlight_location pp_answer s =
  JsooTop.execute printval ?pp_code:pp_code ?highlight_location:highlight_location pp_answer s