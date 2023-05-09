module Cell = struct 
  include Int32
  let of_int = Int32.of_int 
  let true_ = of_int (-1) 
  let false_ = of_int (0) 
end 

type t = {
  stack : Cell.t list; 
  rstack : Cell.t list; 
  dict : (string, t -> t) Hashtbl.t; 
}

let create () = 
  let dict = Hashtbl.create 10 in 
  {
    stack = [];
    rstack = [];
    dict = dict; 
  }

let pop s = match s.stack with 
  | i :: il -> i, {s with stack = il}
  | _ -> raise Error.StackError 

let pop2 s = 
  let n2, s' = pop s in 
  let n1, s'' = pop s' in 
  n1, n2, s'' 

let pop3 s = 
  let n3, s' = pop s in 
  let n2, s'' = pop s' in 
  let n1, s''' = pop s'' in 
  n1, n2, n3, s'''

let push s i = {s with stack = i :: s.stack}

let rpop s = match s.rstack with 
  | r :: rl -> r, {s with rstack = rl}
  | _ -> raise Error.RStackError 

let rpush s r = {s with rstack = r :: s.rstack}

let has_word s w = Hashtbl.mem s.dict w 

