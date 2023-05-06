type func = State.t -> State.t 

type t = (string, func) Hashtbl.t 

let create () : (string, func) Hashtbl.t = Hashtbl.create 10 

let load_builtins (dict : t) = 
  List.iter (fun (s, f) -> Hashtbl.add dict s f) Builtins.builtins


