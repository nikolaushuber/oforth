type t = int * int 

let ( @@ ) a b = 
  let a1, a2 = a in 
  let b1, b2 = b in 
  let diff = a2 - b1 in 
  if diff > 0 then 
    a1, b2 + diff 
  else
    a1 + diff, b2 


let builtins = [
  (* Arithmetic *)
  "+", (2, 1); 
  "-", (2, 1); 
  "*", (2, 1);
  "/", (2, 1); 
  "mod", (2, 1); 
  "/mod", (2, 2); 
  "*/mod", (3, 2); 
  "2*", (1, 1); 
  "2/", (1, 1);
  "1+", (1, 1); 
  "1-", (1, 1); 
  "max", (2, 1); 
  "min", (2, 1); 
  "abs", (1, 1); 
  "negate", (1,1); 

  (* Logic *)
  "0=", (1, 1); 
  "<", (2, 1); 
  ">", (2, 1); 
  "and", (2, 1); 
  "or", (2, 1); 
  "true", (0, 1); 
  "false", (0, 1); 
  "lshift", (2, 1); 
  "rshift", (2, 1); 
  "invert", (1, 1); 
  "rot", (3, 3); 

  (* Stack *)
  "dup", (1,2); 
  "swap", (2, 2); 
  "drop", (1, 0); 
  "over", (2, 3); 
]
