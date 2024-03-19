val setup_toplevel : unit -> unit

val execute : 
    pp_code:Format.formatter ->
    ?highlight_location:(Warnings.loc -> unit) ->
    Format.formatter ->
    string ->
    unit