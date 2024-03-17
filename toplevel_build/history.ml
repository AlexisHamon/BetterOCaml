open Js_of_ocaml

let js_get_s () : Dom_html.storage Js.t =
  match Js.Optdef.to_option Dom_html.window##.localStorage with
  | exception _ -> raise Not_found
  | None -> raise Not_found
  | Some t -> t

type history = {data: string array; idx: int; loc: string}

let set_storage (h: history) : unit =
  try
    let s = js_get_s () in
    let str = Json.output h.data in
    s##setItem (Js.string ("toplevelhistory_"^h.loc)) str
  with Not_found -> ()

let get_storage (loc: string) : history option = 
  try
    let s = js_get_s () in
    match Js.Opt.to_option (s##getItem (Js.string ("toplevelhistory_"^loc))) with
    | None -> raise Not_found
    | Some s -> let a = Json.unsafe_input s in
        Some({data=a; idx = Array.length a; loc=loc})
  with _ -> None

let empty (loc: string) : history = {data=[|""|]; idx=0; loc=loc}

let setup (loc: string) : history =
  match get_storage loc with
    | None -> empty (loc)
    | Some(h) -> h

let push (h: history) (text: string) : history =
  match Array.length h.data, text with
    | len, "" -> {data=h.data; idx=len - 1; loc=h.loc}
    | 1, text -> 
      let h = {data=[| text; "" |]; idx=1; loc=h.loc} in
      set_storage h; h 
    | len, text when text <> h.data.(len - 2) ->
      let data = Array.append h.data [| "" |] in
      data.(len - 2) <- text;
      let h = {data=data; idx=len; loc=h.loc} in
      set_storage h; h
    | len, _ -> {data=h.data; idx=len - 1; loc=h.loc}

let save_current (h: history) (text: string) : unit = h.data.(h.idx) <- text

let previous (h: history) textbox : history =
  if h.idx > 0 then (
    textbox##.value := Js.string h.data.(h.idx-1);
    {data=h.data; idx=h.idx-1; loc=h.loc}
  ) else (
    h
  )

let next (h: history) textbox : history =
  if h.idx < Array.length h.data - 1 then (
    textbox##.value := Js.string h.data.(h.idx+1);
    {data=h.data; idx=h.idx+1; loc=h.loc}
  ) else (
    h
  )