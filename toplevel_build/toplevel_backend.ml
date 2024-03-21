open Js_of_ocaml
open Js_of_ocaml_toplevel

let buffer = Buffer.create(100)
let stdout_buffer = Buffer.create(100)
let stderr_buffer = Buffer.create(100)
let formatter = Format.formatter_of_buffer buffer
let stderr_formatter = Format.formatter_of_buffer stderr_buffer


let warnings_l = ref []

let () =
  Location.warning_reporter :=
    (fun loc w ->
       match Warnings.report w with
       | `Inactive -> Location.default_warning_reporter loc w
       | `Active { Warnings.id = _; message; is_error = _; sub_locs } ->
           let r = (loc, message), sub_locs in
           warnings_l := r :: !warnings_l;
           Location.default_warning_reporter loc w)


module Ppx_support = struct
  let init () = Ast_mapper.register "js_of_ocaml" (fun _ -> Ppx_js.mapper)
end

open Js_of_ocaml_compiler.Stdlib

let refill_lexbuf s p ppf buffer len =
  if !p = String.length s
  then 0
  else
    let len', nl =
      try String.index_from s !p '\n' - !p + 1, false
      with _ -> String.length s - !p, true
    in
    let len'' = min len len' in begin
    Bytes.blit_string ~src:s ~src_pos:!p ~dst:buffer ~dst_pos:0 ~len:len'';
    let s = Bytes.sub_string buffer ~pos:0 ~len:len'' in
    if nl then ppf (s^"\n") else ppf s;
    p := !p + len'';
    len''
    end

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


let drainBuffer bf = 
  let content = Buffer.contents(bf) in
  Buffer.clear(bf);
  content

type report = {loc: Location.t option; code: string list; value: string; stdout: string; stderr: string}

let report ?loc ?value ?stdout ?stderr ?code () = 
  {
    loc    = loc;
    code   = Option.value code   ~default:([]);
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
  warnings_l := [];
  
  (* let lexbuf = Lexing.from_string code in *)
  let code_s = ref [] in
  let ppf s = code_s := s :: !code_s in
  let rd_cd () = let v = !code_s in code_s := []; v in 
  let lexbuf = Lexing.from_function (refill_lexbuf code (ref 0) ppf) in
  Location.input_lexbuf := Some lexbuf;
  let rec run out_messages =
    match parse_toplevel_phrase lexbuf with
      | Error End_of_file -> out_messages
      | Error exn ->
        Errors.report_error stderr_formatter exn;
        begin match JsooTopError.loc exn with
          | None -> Error (exn, report ~code:(rd_cd ()) ()) :: out_messages
          | Some loc -> Error (exn, report ~code:(rd_cd ()) ~loc:loc ()) :: out_messages
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
          run ((Ok (report ~code:(rd_cd ()) ?loc:loc ())) :: out_messages)
        
        | Ok(false) ->
          run ((Error (RuntimeError, report ~code:(rd_cd ()) ?loc:loc ~stderr:"Uncaught exception: RuntimeError" ())) :: out_messages)
        | Error(Sys.Break) -> (Error (Sys.Break, report ~code:(rd_cd ()) ?loc:None ~stderr:"Interupted" ())) :: out_messages
        | Error(exn) ->
          let _ = try Errors.report_error stderr_formatter exn with _ -> () in
          (Error (exn, report ~code:(rd_cd ()) ?loc:(JsooTopError.loc exn) ())) :: out_messages
        in List.rev (run [])
        
let eval_silent ev s = 
  ev s

let execute ~pp_code ~pp_value ~pp_stdout ~pp_stderr ?highlight_location s =
  let response = eval s in

  let pp_report (report: report) =
    let {code=code; value=value; stdout=stdout; stderr=stderr; _} = report in
    List.iter ~f:(fun s -> if (String.length s > 0) then pp_code s) (List.rev code);
    if (String.length value > 0)  then pp_value  value;
    if (String.length stdout > 0) then pp_stdout stdout;
    if (String.length stderr > 0) then pp_stderr stderr in

  let try_apply f_opt t x_opt = 
    match f_opt, x_opt with
      | None, _ | _, None -> ()
      | Some f, Some x -> f t x in
  let aux element =
    (match element with
      | Ok report -> pp_report report
      | Error (_exn, report) ->
        pp_report report;
        try_apply highlight_location Colorize.Highlight_error report.loc
    ) in
    List.iter ~f:aux response;
    List.iter ~f:(fun ((loc, _), _) -> try_apply highlight_location Colorize.Highlight_warning (Some loc)) (!warnings_l)


let setup_toplevel ev =
  JsooTop.initialize ();

  Format.pp_set_margin formatter 80;
  Format.pp_set_max_indent formatter 70;
  Sys_js.set_channel_flusher stdout (Buffer.add_string stdout_buffer);
  Sys_js.set_channel_flusher stderr (Buffer.add_string stderr_buffer);

  Sys.interactive := false;
  if Version.comp Version.current [ 4; 07 ] >= 0 then eval_silent ev "open Stdlib;;";
  eval_silent ev "print_string (\"        OCaml version \" ^ Sys.ocaml_version);;";
  eval_silent ev "#enable \"pretty\";;";
  eval_silent ev "#disable \"shortvar\";;";
  eval_silent ev "#directory \"/static\";;";
  (* exec' "module Num = Big_int_Z;;"; **)
  Ppx_support.init ();
  let[@alert "-deprecated"] new_directive n k = Hashtbl.add Toploop.directive_table n k in
    new_directive
    "load_js"
    (Toploop.Directive_string (fun name -> Js.Unsafe.global##load_script_ name));
  Sys.interactive := true;
  ()
  