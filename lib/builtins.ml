open State 

let add s = 
  let a, s' = pop s in 
  let b, s'' = pop s' in 
  push s'' (a + b) 

let sub s = 
  let a, s' = pop s in 
  let b, s'' = pop s' in 
  push s'' (b - a)

let mul s =  
  let a, s' = pop s in 
  let b, s'' = pop s' in 
  push s'' (a * b)

let div s = 
  let a, s' = pop s in 
  let b, s'' = pop s' in 
  push s'' (b / a) 

let show_stack s = 
  print_string "["; 
  List.iter (fun i -> print_string (" " ^ (string_of_int i))) (List.rev s.stack);
  print_endline " ]";
  s

let dot s = 
  let i, s' = pop s in 
  print_endline ((string_of_int i)); 
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

let builtins = [
  "+", add; 
  "-", sub; 
  "*", mul;
  "/", div; 
  ".s", show_stack; 
  ".", dot; 
  "bye", bye; 
  "cr", cr; 
  "r>", r_from; 
  ">r", r_to; 
]
