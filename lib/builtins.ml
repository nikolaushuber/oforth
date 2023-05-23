open State 

let add s = 
  let n1, n2, s' = pop2 s in 
  push s' (Cell.add n1 n2) 

let sub s = 
  let n1, n2, s' = pop2 s in 
  push s' (Cell.sub n1 n2)

let mul s =  
  let n1, n2, s' = pop2 s in 
  push s' (Cell.mul n1 n2) 

let two_mul s = 
  let a, s' = pop s in 
  push s' (Cell.mul (Cell.of_int 2) a) 

let mod_ s = 
  let n1, n2, s' = pop2 s in 
  push s' (Cell.rem n1 n2)

let slash_mod s = 
  let n1, n2, s' = pop2 s in 
  let n3 = Cell.rem n1 n2 in 
  let n4 = Cell.div n1 n2 in 
  let s'' = push s' n3 in 
  push s'' n4 

let two_div s = 
  let a, s' = pop s in 
  push s' (Cell.div a (Cell.of_int 2)) 

let abs_ s = 
  let n1, s' = pop s in 
  push s' (Cell.abs n1) 

let negate s = 
  let n1, s' = pop s in 
  push s' (Cell.neg n1) 

let star_slash_mod s = 
  let n1, n2, n3, s' = pop3 s in 
  let d = Cell.mul n1 n2 in 
  let n4 = Cell.rem d n3 in 
  let n5 = Cell.div d n3 in 
  let s'' = push s' n4 in 
  push s'' n5 

let one_plus s = 
  let n1, s' = pop s in 
  push s' (Cell.succ n1) 

let one_minus s = 
  let n1, s' = pop s in 
  push s' (Cell.pred n1) 

let div s = 
  let n1, n2, s' = pop2 s in 
  push s' (Cell.div n1 n2)  

let show_stack s = 
  print_string "["; 
  List.iter (fun i -> print_string (" " ^ (Cell.to_string i))) (List.rev s.stack);
  print_endline " ]";
  s

let dot s = 
  let i, s' = pop s in 
  print_endline ((Cell.to_string i)); 
  s'

let cr s = 
  print_endline ""; 
  s

let dup s = 
  let top, _ = pop s in 
  push s top 

let bye _ = raise Error.ByeException

let r_from s = 
  let top, s' = rpop s in 
  push s' top 

let r_to s = 
  let top, s' = pop s in 
  rpush s' top 

let swap s = 
  let n1, n2, s' = pop2 s in 
  let s'' = push s' n2 in 
  push s'' n1 

let drop s = 
  let _, s' = pop s in 
  s' 

let over s =
  let x1, _, _ = pop2 s in 
  push s x1 

let zero_equals s = 
  let a, s' = pop s in 
  push s' (if a == Cell.of_int 0 then Cell.true_ else Cell.false_)

let less_than s = 
  let n1, n2, s' = pop2 s in 
  push s' (if n1 < n2 then Cell.true_ else Cell.false_)

let greater_than s = 
  let n1, n2, s' = pop2 s in 
  push s' (if n1 > n2 then Cell.true_ else Cell.false_)

let and_ s = 
  let n1, n2, s' = pop2 s in  
  push s' (Cell.logand n1 n2) 

let or_ s = 
  let n1, n2, s' = pop2 s in 
  push s' (Cell.logor n1 n2) 

let true_ s = push s Cell.true_ 

let false_ s = push s Cell.false_ 

let lshift s = 
  let x1, u, s' = pop2 s in 
  push s' (Cell.shift_left x1 (Cell.to_int u))

let rshift s = 
  let x1, u, s' = pop2 s in 
  push s' (Cell.shift_right x1 (Cell.to_int u)) 

let invert s = 
  let x1, s' = pop s in 
  push s' (Cell.lognot x1)

let rot s = 
  let x1, x2, x3, s' = pop3 s in 
  let s'' = push s' x2 in 
  let s''' = push s'' x3 in 
  push s''' x1 

let max_ s = 
  let n1, n2, s' = pop2 s in 
  push s' (Cell.max n1 n2) 

let min_ s = 
  let n1, n2, s' = pop2 s in 
  push s' (Cell.min n1 n2) 

let builtins = [
  (* Arithmetic *)
  "+", add; 
  "-", sub; 
  "*", mul;
  "/", div; 
  "mod", mod_; 
  "/mod", slash_mod; 
  "*/mod", star_slash_mod; 
  "2*", two_mul; 
  "2/", two_div;
  "1+", one_plus; 
  "1-", one_minus; 
  "max", max_; 
  "min", min_; 
  "abs", abs_; 
  "negate", negate; 

  (* Logic *)
  "0=", zero_equals; 
  "<", less_than; 
  ">", greater_than; 
  "and", and_; 
  "or", or_; 
  "true", true_; 
  "false", false_; 
  "lshift", lshift; 
  "rshift", rshift; 
  "invert", invert; 
  "rot", rot; 

  (* Stack *)
  "dup", dup; 
  "swap", swap; 
  "r>", r_from; 
  ">r", r_to;
  "drop", drop; 
  "over", over; 

  (* Tools *)
  ".s", show_stack; 
  ".", dot; 
  "bye", bye; 
  "cr", cr; 
]
