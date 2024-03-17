val setup_toplevel : unit -> unit

val execute : 
    bool ->
    ?pp_code:Format.formatter ->
    ?highlight_location:(Location.t -> unit) ->
    Format.formatter ->
    string ->
    unit