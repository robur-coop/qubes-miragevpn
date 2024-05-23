type finaliser = unit -> unit
type t = { mutable finalisers : finaliser list }

let create () = { finalisers= [] }
let add ~finaliser t = t.finalisers <- finaliser :: t.finalisers
let apply fn = fn ()
let finalise t = List.iter apply t.finalisers
