open Helpers 

exception SyntaxError 

type t = 
  | Word of string 
  | If of (t * t) 
  | Block of t list 
  | Do of t 

let rec to_ast0 input acc = match input with  
  | "if" :: rest -> begin 
    (* Look for closest else *)
    let else_pos = find_tok rest "else" in 
    begin match else_pos with 
    | Some e_pos -> 
      (* There is an else clause *)
      let then_tkns, rest = split_list rest e_pos in 
      let then_pos = find_tok_last rest "then" in 
      let else_tkns, rest = begin match then_pos with 
        | Some t_pos -> split_list rest t_pos 
        | None -> raise SyntaxError 
      end in 
      to_ast0 rest (If (to_ast0 then_tkns [], to_ast0 else_tkns []) :: acc) 
    | None -> 
      (* There is no else clause *)
      let then_pos = find_tok_last rest "then" in 
      let tkns, rest = begin match then_pos with 
        | Some t_pos -> split_list rest t_pos 
        | None -> raise SyntaxError 
      end in 
      to_ast0 rest (If (to_ast0 tkns [], Block []) :: acc)  
    end
  end
  | "do" :: rest -> begin 
    (* Find furthest loop *)
    let loop_pos = find_tok rest "loop" in 
    begin match loop_pos with 
      | Some l_pos -> 
        let body_tkns, rest = split_list rest l_pos in 
        to_ast0 rest (Do (to_ast0 body_tkns []) :: acc) 
      | None -> raise SyntaxError 
    end 
  end
  | word :: rest -> to_ast0 rest ((Word word) :: acc) 
  | [] -> Block (List.rev acc) 

let parse input = 
  try
    Some (to_ast0 input [])
  with 
    | SyntaxError -> None 
