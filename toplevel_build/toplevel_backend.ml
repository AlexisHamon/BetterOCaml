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
  (* exec' "module Num = Big_int_Z;;"; **)
  Ppx_support.init ();
  let[@alert "-deprecated"] new_directive n k = Hashtbl.add Toploop.directive_table n k in
    new_directive
    "load_js"
    (Toploop.Directive_string (fun name -> Js.Unsafe.global##load_script_ name));
  Sys.interactive := true;
  ()

open Js_of_ocaml_compiler.Stdlib

let refill_lexbuf s p ppf buffer len =
  if !p = String.length s
  then 0
  else
    let len', nl =
      try String.index_from s !p '\n' - !p + 1, false
      with _ -> String.length s - !p, true
    in
    let len'' = min len len' in
    Bytes.blit_string ~src:s ~src_pos:!p ~dst:buffer ~dst_pos:0 ~len:len'';
    (match ppf with
    | Some ppf ->
        Format.fprintf ppf "%s" (Bytes.sub_string buffer ~pos:0 ~len:len'');
        if nl then Format.pp_print_newline ppf ();
        Format.pp_print_flush ppf ()
    | None -> ());
    p := !p + len'';
    len''

module JsooTopPpx = struct
  let ppx_rewriters = ref []

  let () = Ast_mapper.register_function := fun _ f -> ppx_rewriters := f :: !ppx_rewriters

  let preprocess_structure str =
    let open Ast_mapper in
    List.fold_right !ppx_rewriters ~init:str ~f:(fun ppx_rewriter str ->
        let mapper = ppx_rewriter [] in
        mapper.structure mapper str)

  let _preprocess_signature str =
    let open Ast_mapper in
    List.fold_right !ppx_rewriters ~init:str ~f:(fun ppx_rewriter str ->
        let mapper = ppx_rewriter [] in
        mapper.signature mapper str)

  let preprocess_phrase phrase =
    let open Parsetree in
    match phrase with
    | Ptop_def str -> Ptop_def (preprocess_structure str)
    | Ptop_dir _ as x -> x
end

module JsooTopError = struct
  let loc = function
    | Syntaxerr.Error x -> Some (Syntaxerr.location_of_error x)
    | Lexer.Error (_, loc)
    | Typecore.Error (loc, _, _)
    | Typetexp.Error (loc, _, _)
    | Typeclass.Error (loc, _, _)
    | Typemod.Error (loc, _, _)
    | Typedecl.Error (loc, _)
    | Translcore.Error (loc, _)
    | Translclass.Error (loc, _)
    | Translmod.Error (loc, _) -> Some loc
    | _ -> None
end

let buffer = Buffer.create(100)
let stdout_buffer = Buffer.create(100)
let stderr_buffer = Buffer.create(100)
let stdstr_buffer = Buffer.create(100)
let formatter = Format.formatter_of_buffer buffer
let stdstr_formatter = Format.formatter_of_buffer stdstr_buffer
let stderr_formatter = Format.formatter_of_buffer stderr_buffer

let drainBuffer bf = 
  let content = Buffer.contents(bf) in
  Buffer.clear(bf);
  content

type report = {loc: Location.t option; code: string; value: string; stdout: string; stderr: string}

let report ?loc ?value ?stdout ?stderr ?code () = 
  {
    loc    = loc;
    code   = Option.value code   ~default:(drainBuffer stdstr_buffer);
    value  = Option.value value  ~default:(drainBuffer buffer);
    stdout = Option.value stdout ~default:(drainBuffer stdout_buffer);
    stderr = Option.value stderr ~default:(drainBuffer stderr_buffer);
  } 

let parse_toplevel_phrase lexbuf =
  try (Ok(!Toploop.parse_toplevel_phrase lexbuf)) with
  | exn -> Error(exn)

let execu_toplevel_phrase phrase =
  try (Ok(Toploop.execute_phrase true formatter phrase)) with
        | exn -> Error(exn) 

module List = struct
  include List
  let ls l = 
    match List.last l with
      | Some v -> v
      | None   -> raise (Failure "L'élément n'a pas été trouvé !")
end


let loc_of_phrase phrase = 
  match phrase with
    | Parsetree.Ptop_def [] | Ptop_dir _ -> None
    | Parsetree.Ptop_def l  -> 
      let loc = {
          Location.loc_start = (List.hd l).pstr_loc.loc_start;
          Location.loc_end   = (List.ls l).pstr_loc.loc_end;
          Location.loc_ghost = false } in 
      Some loc

exception RuntimeError
let eval code =
  Buffer.clear buffer;
  Buffer.clear stderr_buffer;
  Buffer.clear stdout_buffer;
  Buffer.clear stdstr_buffer;
  
  (* let lexbuf = Lexing.from_string code in *)
  let lexbuf = Lexing.from_function (refill_lexbuf code (ref 0) (Some stdstr_formatter)) in
  Location.input_lexbuf := Some lexbuf;
  let rec run out_messages =
    Buffer.clear stdstr_buffer;
    match parse_toplevel_phrase lexbuf with
      | Error End_of_file -> out_messages
      | Error exn -> 
        Errors.report_error stderr_formatter exn;
        begin match JsooTopError.loc exn with
          | None -> Error (exn, report ()) :: out_messages
          | Some loc -> Error (exn, report ~loc:loc ()) :: out_messages
        end
      | Ok ph ->
        Buffer.clear buffer;
        Buffer.clear stderr_buffer;
        Buffer.clear stdout_buffer;

        (* Pprintast.top_phrase stdstr_formatter ph; *)
        let loc = loc_of_phrase ph in 
    
        let ph = JsooTopPpx.preprocess_phrase ph in
          
        match execu_toplevel_phrase ph with
        | Ok(true) ->
          run ((Ok (report ?loc:loc ())) :: out_messages)
        
        | Ok(false) ->
          run ((Error (RuntimeError, report ?loc:loc ~stderr:"Uncaught exception: RuntimeError" ())) :: out_messages)
        | Error(Sys.Break) -> (Error (Sys.Break, report ?loc:None ~stderr:"Interupted" ())) :: out_messages
        | Error(exn) -> 
          Errors.report_error stderr_formatter exn;
          (Error (exn, report ?loc:(JsooTopError.loc exn) ())) :: out_messages
        in List.rev (run [])
        

let execute ~pp_code ?highlight_location pp_answer s =
  let response = eval s in

  let fflush_all () = 
    Format.pp_print_flush pp_code ();
    Format.pp_print_flush pp_answer ();
    Format.pp_print_flush Format.std_formatter ();
    Format.pp_print_flush Format.err_formatter () in

  let try_apply f_opt x_opt = 
    match f_opt, x_opt with
      | None, _ | _, None -> ()
      | Some f, Some x -> f x in
  let aux element =
    (match element with
      | Ok {value=value; code=code; _} -> 
        Format.fprintf pp_code "%s" code; 
        Format.fprintf pp_answer "%s" value;
        fflush_all ();
      | Error (_exn, {loc=loc; value=value; stdout=_stdout; stderr=stderr; code=code}) ->
        Format.fprintf pp_code "%s" code;
        Format.fprintf pp_answer "%s" value;
        fflush_all ();
        Format.fprintf Format.err_formatter "%s" stderr;
        fflush_all ();
        try_apply highlight_location loc
    ) in
    List.iter ~f:aux response
      
  
 