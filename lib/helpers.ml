let rec find_tok' lst tok c = match lst with 
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

