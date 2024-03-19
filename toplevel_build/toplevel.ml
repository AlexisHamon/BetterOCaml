open Js_of_ocaml
open Js_of_ocaml_tyxml
open Lwt


(* General functions *)

let by_id s = Dom_html.getElementById s

let by_id_coerce s f = Js.Opt.get (f (Dom_html.getElementById s)) (fun () -> raise Not_found)

(* load file using a synchronous XMLHttpRequest *)
let load_resource_aux filename url =
  Js_of_ocaml_lwt.XmlHttpRequest.perform_raw ~response_type:XmlHttpRequest.ArrayBuffer url
  >|= fun frame ->
  if frame.Js_of_ocaml_lwt.XmlHttpRequest.code = 200
  then
    Js.Opt.case
      frame.Js_of_ocaml_lwt.XmlHttpRequest.content
      (fun () -> Printf.eprintf "Could not load %s\n" filename)
      (fun b ->
        Sys_js.update_file ~name:filename ~content:(Typed_array.String.of_arrayBuffer b))
  else ()

let load_resource scheme ~prefix ~path:suffix =
  let url = scheme ^ suffix in
  let filename = Filename.concat prefix suffix in
  Lwt.async (fun () -> load_resource_aux filename url);
  Some ""

let setup_pseudo_fs () =
  Sys_js.mount ~path:"/dev/" (fun ~prefix:_ ~path:_ -> None);
  Sys_js.mount ~path:"/http/" (load_resource "http://");
  Sys_js.mount ~path:"/https/" (load_resource "https://");
  Sys_js.mount ~path:"/ftp/" (load_resource "ftp://");
  Sys_js.mount ~path:"/home/" (load_resource "filesys/")

let resize ~container ~textbox () =
  Lwt.pause ()
  >>= fun () ->
  textbox##.style##.height := Js.string "auto";
  textbox##.style##.height
  := Js.string (Printf.sprintf "%dpx" (max 18 textbox##.scrollHeight));
  container##.scrollTop := container##.scrollHeight;
  Lwt.return ()

let rec iter_on_sharp ~f x =
  Js.Opt.iter (Dom_html.CoerceTo.element x) (fun e ->
      if Js.to_bool (e##.classList##contains (Js.string "sharp")) then f e);
  match Js.Opt.to_option x##.nextSibling with
  | None -> ()
  | Some n -> iter_on_sharp ~f n

let current_position = ref 0

let highlight_location loc =
  let x = ref 0 in
  let output = by_id "output" in
  let first =
    Js.Opt.get (output##.childNodes##item !current_position) (fun _ -> assert false)
  in
  iter_on_sharp first ~f:(fun e ->
      incr x;
      let _file1, line1, col1 = Location.get_pos_info loc.Location.loc_start in
      let _file2, line2, col2 = Location.get_pos_info loc.Location.loc_end in
      if !x >= line1 && !x <= line2
      then
        let from_ = if !x = line1 then `Pos col1 else `Pos 0 in
        let to_ = if !x = line2 then `Pos col2 else `Last in
        Colorize.highlight from_ to_ e)

let append colorize output cl (s:string) =
  if String.length s > 0 then
    Dom.appendChild output (Tyxml_js.To_dom.of_element (colorize ~a_class:cl s))
  else 
    Dom.appendChild output (Tyxml_js.To_dom.of_element (colorize ~a_class:cl "empty output"))

let append_to_console s =
  Firebug.console##log (Js.string s)

let sanitize_command cmd = 
  let len = String.length cmd in
    if try cmd <> "" && cmd.[len - 1] <> ';' && cmd.[len - 2] <> ';'
      with _ -> true
    then cmd ^ ";;"
    else if try cmd <> "" && cmd.[len - 1] = ';' && cmd.[len - 2] <> ';'
      with _ -> true
    then cmd ^ ";"
    else cmd

let string_to_void (_:string) : unit = ()

let execute_toplevel = 
  let output = by_id "output" in
  Toplevel_backend.execute
    ~pp_code:  (append Colorize.ocaml output "sharp")
    ~pp_value: (append Colorize.ocaml output "caml")
    ~pp_stdout:(append Colorize.text output "stdout")
    ~pp_stderr:(append Colorize.text output "stderr")
    ~highlight_location

let clear_toplevel () =
  (by_id "output")##.innerHTML := Js.string "";
  ()

let reset_toplevel () =
  clear_toplevel ();
  Toplevel_backend.setup_toplevel execute_toplevel;
  ()

let execute_callback mode content =
  let output = by_id "output" in
  current_position := output##.childNodes##.length;
  let content' = sanitize_command content in
  match mode with
    |"internal" -> 
    Toplevel_backend.execute 
      ~pp_code:  string_to_void
      ~pp_value: string_to_void
      ~pp_stdout:(append Colorize.text output "stdout")
      ~pp_stderr:(append Colorize.text output "stderr")
      ~highlight_location content'
    |"console"  -> 
    Toplevel_backend.execute 
      ~pp_code:  string_to_void
      ~pp_value: append_to_console
      ~pp_stdout:(append Colorize.text output "stdout")
      ~pp_stderr:(append Colorize.text output "stderr")
      ~highlight_location content'
    |"toplevel" -> 
    Toplevel_backend.execute
      ~pp_code:  (append Colorize.ocaml output "sharp")
      ~pp_value: (append Colorize.ocaml output "caml")
      ~pp_stdout:(append Colorize.text output "stdout")
      ~pp_stderr:(append Colorize.text output "stderr")
      ~highlight_location content'
    |_ -> ()

let run _ =
  let container = by_id "toplevel-container" in
  let textbox : 'a Js.t = by_id_coerce "userinput" Dom_html.CoerceTo.textarea in
  let h = ref (History.setup "base") in
  let execute () =
    let content = Js.to_string textbox##.value##trim in
    h := History.push !h content;
    textbox##.value := Js.string "";
    execute_callback "toplevel" content;
    resize ~container ~textbox () >>= fun () -> container##.scrollTop := container##.scrollHeight;
    Lwt.return_unit
  in
  let history_down _e =
    let txt = Js.to_string textbox##.value in
    let pos = textbox##.selectionStart in
    try
      if String.length txt = pos then raise Not_found;
      let _ = String.index_from txt pos '\n' in
      Js._true
    with Not_found ->
      History.save_current !h txt;
      h := History.next !h textbox;
      Js._false
  in
  let history_up _e =
    let txt = Js.to_string textbox##.value in
    let pos = textbox##.selectionStart - 1 in
    try
      if pos < 0 then raise Not_found;
      let _ = String.rindex_from txt pos '\n' in
      Js._true
    with Not_found ->
      History.save_current !h txt;
      h := History.previous !h textbox;
      Js._false
  in
  let meta e =
    let b = Js.to_bool in
    b e##.ctrlKey || b e##.altKey || b e##.metaKey
  in
  let shift e = Js.to_bool e##.shiftKey in
  (* setup handlers *)
  textbox##.onkeyup :=
    Dom_html.handler (fun _ ->
        Lwt.async (resize ~container ~textbox);
        Js._true);
  textbox##.onchange :=
    Dom_html.handler (fun _ ->
        Lwt.async (resize ~container ~textbox);
        Js._true);
  textbox##.onkeydown :=
    Dom_html.handler (fun e ->
        match e##.keyCode with
        | 13 when not (meta e || shift e) ->
            Lwt.async execute;
            Js._false
        | 13 ->
            Lwt.async (resize ~container ~textbox);
            Js._true
        | 09 ->
            Indent.textarea textbox;
            Js._false
        | 75 when meta e ->
            clear_toplevel ();
            Js._false
        | 76 when meta e ->
            reset_toplevel ();
            Js._false
        | 38 -> history_up e
        | 40 -> history_down e
        | _ -> Js._true);
  (Lwt.async_exception_hook :=
     fun exc ->
       Format.eprintf "exc during Lwt.async: %s@." (Printexc.to_string exc);
       match exc with
        | Js_error.Exn e -> let e = Js_error.to_error e in Firebug.console##log e##.stack
        | _ -> ());
  Lwt.async (fun () ->
      resize ~container ~textbox ()
      >>= fun () ->
      textbox##focus;
      Lwt.return_unit);
  let readline () =
    Js.Opt.case
      (Dom_html.window##prompt (Js.string "The toplevel expects inputs:") (Js.string ""))
      (fun () -> "\n")
      (fun s -> Js.to_string s ^ "\n")
  in
  Sys_js.set_channel_filler stdin readline;
  setup_pseudo_fs ();
  Toplevel_backend.setup_toplevel execute_toplevel;
  textbox##.value := Js.string ""


(* Init and create callbacks *)

let _ =
  Dom_html.window##.onload :=
    Dom_html.handler (fun _ -> run (); Js._false);
  
  (* Add callbacks *)
  Js.Unsafe.global##.executecallback := (object%js
      val execute = Js.wrap_meth_callback
          (fun _ mode content -> execute_callback (Js.to_string mode) (Js.to_string content))
      val create_file = Js.wrap_meth_callback
          (fun _ name content -> Sys_js.create_file ~name:(Js.to_string name) ~content:(Js.to_string content))
      val update_file = Js.wrap_meth_callback
          (fun _ name content -> Sys_js.update_file ~name:(Js.to_string name) ~content:(Js.to_string content))
    end);
  Js.Unsafe.global##.toplevelcallback := (object%js
      val setup = Js.wrap_meth_callback
          (fun () -> Toplevel_backend.setup_toplevel execute_toplevel)
      val clear = Js.wrap_meth_callback
          (fun () -> clear_toplevel ())
      val reset = Js.wrap_meth_callback
          (fun () -> reset_toplevel ())
    end);
