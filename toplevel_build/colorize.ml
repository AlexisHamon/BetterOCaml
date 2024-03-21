open Js_of_ocaml
open Js_of_ocaml_tyxml


let lexeme = Sedlexing.Utf8.lexeme;;

let binary = [%sedlex.regexp? "0b", Plus('0' | '1'), Star('0' | '1' | '_')]
let octal = [%sedlex.regexp? "0o", Plus('0'..'7'), Star('0'..'7' | '_')]
let decimal = [%sedlex.regexp? Plus('0'..'9'), Star('0'..'9' | '_'), Opt('.'), Star('0'..'9' | '_')]
let hexadecimal = [%sedlex.regexp? "0x", Plus('0'..'9' | 'A'..'F' | 'a'..'f'), Star('0'..'9' | 'A'..'F' | 'a'..'f' | '_')]
let scientific = [%sedlex.regexp? Plus(decimal), ('e' | 'E'), Opt('+' | '-'), Plus('0'..'9' | '_')]

let numeric = [%sedlex.regexp? binary | octal | decimal | hexadecimal | scientific]

let boolean = [%sedlex.regexp? "true" | "false"]
let echar = [%sedlex.regexp? 't' | 'b' | 'n' | 'r' | 'f' | '\\' | '"' | '\'']

let escaped_char = [%sedlex.regexp? '\\', echar]
let string = [%sedlex.regexp? '"', Star(Compl(0x22) | escaped_char),'"']
let char = [%sedlex.regexp? "'", (Compl(0x27) | escaped_char), "'"]

let space = [%sedlex.regexp? Plus(' ' | '\n' | '\t' | '\r') ]

let capchar = [%sedlex.regexp? 'A'..'Z']
let lowchar = [%sedlex.regexp? 'a'..'z']
let idchar = [%sedlex.regexp? lowchar | capchar | '0'..'9' | '_']

let modname = [%sedlex.regexp? capchar, Star(idchar)]

let comment = [%sedlex.regexp? "(*", (Star(Compl(0x2A) | ('*', Compl(')')))), "*)"]

let id = [%sedlex.regexp? ('_' | lowchar), Star(idchar)]
let cap_id = [%sedlex.regexp? capchar, id]
let attr_id = [%sedlex.regexp? (id | cap_id), Star('.', Plus(id | cap_id))]

let percent_id = [%sedlex.regexp? '%', attr_id]

let decl_kw = [%sedlex.regexp? "and" | "class" | "constraint" | "exception" | "external" | "let" | "fun" | "function" | "functor" | "in" | "include" | "inherit" | "initializer" | "method" | "module" | "mutable" | "nonrec" | "of" | "open" | "private" | "rec" | "type" | "val" | "virtual"]

let expr_kw = [%sedlex.regexp? "asr" | "do" | "else" | "for" | "if" | "while" | "as" | "assert" | "begin" | "do" | "done" | "downto" | "else" | "end" | "for" | "if" | "land" | "lazy" | "lor" | "lsl" | "lsr" | "lxor" | "match" | "mod" | "new" | "object" | "or" | "ref" | "sig" | "struct" | "then" | "to" | "try" | "when" | "while" | "with" | "#"]

let type_kw = [%sedlex.regexp? "bool" | "int" | "string" | "list" | "array" | "float" | "char" | "unit"]

let lwt_kw = [%sedlex.regexp? "lwt" | "raise_lwt" | ">>=" | ">>" | "=<<" | "for_lwt" | "assert_lwt" | "match_lwt" | "while_lwt"]
let label = [%sedlex.regexp? '~', id]

let directive = [%sedlex.regexp? Opt('\n', Opt('\r')), '#', lowchar, Star(idchar)]

let text ~a_class:cl s = Tyxml_js.Html.(span ~a:[ a_class [ cl ] ] [ txt s ])
let span' cl s = Tyxml_js.Html.(span ~a:[ a_class [ cl ] ] [ txt s ])

let ocaml ~a_class:cl s =
  let lexbuf = Sedlexing.Utf8.from_string s in
  let rec iter acc = match%sedlex lexbuf with
    | eof -> List.rev acc
    | space -> iter ((span' "text" (lexeme lexbuf)) :: acc)
    | numeric -> iter ((span' "numeric" (lexeme lexbuf)) :: acc)
    | boolean -> iter ((span' "constant" (lexeme lexbuf)) :: acc)
    | directive -> iter ((span' "directive" (lexeme lexbuf)) :: acc)
    | decl_kw -> iter ((span' "kw0" (lexeme lexbuf)) :: acc)
    | expr_kw -> iter ((span' "kw1" (lexeme lexbuf)) :: acc)
    | modname -> iter ((span' "kw2" (lexeme lexbuf)) :: acc)
    | type_kw -> iter ((span' "kw3" (lexeme lexbuf)) :: acc)
    | percent_id -> iter ((span' "kw5" (lexeme lexbuf)) :: acc)
    | lwt_kw -> iter ((span' "kw10" (lexeme lexbuf)) :: acc)
    | label -> iter ((span' "kw4" (lexeme lexbuf)) :: acc)
    | id -> iter ((span' "id" (lexeme lexbuf)) :: acc)
    | string -> iter ((span' "string" (lexeme lexbuf)) :: acc)
    | char -> iter ((span' "string" (lexeme lexbuf)) :: acc)
    | comment -> iter ((span' "comment" (lexeme lexbuf)) :: acc)
    | any -> iter ((span' "text" (lexeme lexbuf)) :: acc)
    | _ -> failwith "Invalid lexer state"
  in
  Tyxml_js.Html.(div ~a:[ a_class [ cl ] ] (iter []))
  
type highlight_type = Highlight_error | Highlight_warning | Highlight_alert
let string_of_highlight = function 
  | Highlight_error -> "errorloc"
  | Highlight_warning -> "warningloc"
  | Highlight_alert -> "alertloc"

let highlight (`Pos from_) to_ e t =
  let _ =
    List.fold_left
      (fun pos e ->
        match Js.Opt.to_option (Dom_html.CoerceTo.element e) with
        | None -> pos
        | Some e ->
            let size = Js.Opt.case e##.textContent (fun () -> 0) (fun t -> t##.length) in
            if pos + size > from_ && (to_ = `Last || `Pos pos < to_)
            then e##.classList##add (Js.string (string_of_highlight t));
            pos + size)
      0
      (Dom.list_of_nodeList e##.childNodes)
  in
  ()