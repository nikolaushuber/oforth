type t = {
  stack : int list; 
  rstack : int list; 
}

let create () = {
  stack = [];
  rstack = [];
}

let pop s = match s.stack with 
  | i :: il -> i, {s with stack = il}
  | _ -> raise Error.StackError 

let push s i = {s with stack = i :: s.stack}

let rpop s = match s.rstack with 
  | r :: rl -> r, {s with rstack = rl}
  | _ -> raise Error.RStackError 

let rpush s r = {s with rstack = r :: s.rstack}

