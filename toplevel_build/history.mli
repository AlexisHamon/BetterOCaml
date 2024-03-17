open Js_of_ocaml

type history = {data: string array; idx: int; loc: string}

val setup : string -> history

val push : history -> string -> history

val save_current : history -> string -> unit

val previous : history -> < value : < set : Js.js_string Js.t -> unit ; .. > Js.gen_prop
; .. >
Js.t ->
    history

val next : history -> < value : < set : Js.js_string Js.t -> unit ; .. > Js.gen_prop
; .. >
Js.t ->
history