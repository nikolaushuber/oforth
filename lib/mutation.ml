open State 

let rec is_flat = function 
  | Word _ -> true
  | Const _ -> true 
  | Block words -> List.fold_left (fun init w -> init && is_flat w) true words 
  | If _ -> false 
  | Do _ -> false 

exception Not_flat 

let rec flatten acc = function 
  | Word w -> Word w :: acc 
  | Const i -> Const i :: acc 
  | Block words -> List.fold_left flatten acc words 
  | If _ -> raise Not_flat 
  | Do _ -> raise Not_flat 

type prog = string list 

