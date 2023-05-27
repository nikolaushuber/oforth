open State 

let add s = 
  let n2, s = pop s in 
  let n1, s = pop s in 
  push s (n1 + n2)

let sub s = 
  let n2, s = pop s in 
  let n1, s = pop s in 
  push s (n1 - n2)

let mul s =  
  let n2, s = pop s in 
  let n1, s = pop s in 
  push s (n1 * n2)

let slash_mod s = 
  let n2, s = pop s in 
  let n1, s = pop s in 
  let n3 = n1 mod n2 in 
  let n4 = n1 / n2 in 
  let s = push s n3 in 
  push s n4 

let div s = 
  let n2, s = pop s in 
  let n1, s = pop s in 
  push s (n1 / n2)  

let dup s = 
  let top, _ = pop s in 
  push s top 

let swap s = 
  let n2, s = pop s in 
  let n1, s = pop s in 
  let s = push s n2 in 
  push s n1 

let drop s = 
  let _, s = pop s in 
  s

let over s =
  let x2, s = pop s in 
  let x1, s = pop s in 
  let s = push s x2 in 
  push s x1 

let equals s = 
  let a, s = pop s in 
  let b, s = pop s in 
  push s (if a = b then -1 else 0)

let less_than s = 
  let n2, s = pop s in 
  let n1, s = pop s in 
  push s (if n1 < n2 then -1 else 0)

let and_ s = 
  let n2, s = pop s in 
  let n1, s = pop s in 
  push s (Int.logand n1 n2) 

let or_ s = 
  let n2, s = pop s in 
  let n1, s = pop s in
  push s (Int.logor n1 n2) 

let true_ s = push s (-1) 

let false_ s = push s 0

let lshift s = 
  let u, s = pop s in 
  let x1, s = pop s in 
  push s (Int.shift_left x1 u)

let rshift s = 
  let u, s = pop s in 
  let x1, s = pop s in 
  push s (Int.shift_right x1 u)

let invert s = 
  let x1, s = pop s in 
  push s (Int.lognot x1)

let rot s = 
  let x3, s = pop s in 
  let x2, s = pop s in 
  let x1, s = pop s in 
  let s = push s x2 in 
  let s = push s x3 in 
  push s x1 

let max_ s = 
  let n2, s = pop s in 
  let n1, s = pop s in 
  push s (Int.max n1 n2) 

let min_ s = 
  let n2, s = pop s in 
  let n1, s = pop s in 
  push s (Int.min n1 n2) 

let nop s = s 

let show_stack s = 
  print_endline (string_of_stack s); s 

let cr s = print_endline ""; s 

let bye _ = exit 0 

let colon s = start_compiling s 
  

let basics = [
  (* Arithmetic *)
  "+", add; 
  "-", sub; 
  "*", mul;
  "/", div; 

  (* Logic *)
  "=", equals; 
  "<", less_than; 
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
  "drop", drop; 
  "over", over; 
]

let immediate = [
  ":", nop; 
  "if", nop; 
  "else", nop; 
  "then", nop; 
  "do", nop; 
  "loop", nop; 
  ";", nop; 
]

let tools = [
  (* Tools *)
  ".s", show_stack; 
  "bye", bye; 
  "cr", cr;  
] 

let builtins = basics @ tools 
