val setup_toplevel : (string -> unit) -> unit

val execute : 
    pp_code:  (string -> unit) ->
    pp_value: (string -> unit) ->
    pp_stdout:(string -> unit) ->
    pp_stderr:(string -> unit) ->
    ?highlight_location:(Colorize.highlight_type -> Warnings.loc -> unit) ->
    string ->
    unit