(* let rec find_tok' lst tok c = match lst with 
  | [] -> None 
  | hd :: tl -> if (String.equal hd tok) then Some c else find_tok' tl tok (c+1) 

let find_tok lst tok = find_tok' lst tok 0 

let find_tok_last lst tok = match find_tok' (List.rev lst) tok 0 with 
  | Some c -> Some (List.length lst - c - 1)
  | None -> None 

let rec split_at n acc l =
    if n = 0 then (List.rev acc, if List.length l == 0 then l else List.tl l) else
    match l with
    | [] -> (List.rev acc, [])
    | h :: t -> split_at (n-1) (h :: acc) t
  
  let split_list l idx = split_at idx [] l

exception SyntaxError 
exception UnknownWord of string 

type ast0 = 
  | Word of string 
  | If of (ast0 * ast0) 
  | Block of ast0 list 
  | Do of ast0 

(* Try to get input into ast0 shape *)

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

let rec ast0_to_ast s ast0 : State.ast = match ast0 with 
  | Block bl -> Block (List.map (ast0_to_ast s) bl) 
  | If (e, t) -> If (ast0_to_ast s e, ast0_to_ast s t) 
  | Do body -> Do (ast0_to_ast s body)
  | Word w -> 
    if State.has_word s w then 
      let f = State.get_word s w in 
      Word f 
    else
      raise (UnknownWord w)

let f s input = 
  let ast0 = to_ast0 input [] in 
  ast0_to_ast s ast0  *)
