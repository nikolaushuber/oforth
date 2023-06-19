let tokenize = Str.split (Str.regexp "[ \r\t\n]+")

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

let core = {|
  : swap      0 rot + ;
  : nip       swap drop ;
  : mod       /mod drop ;
  : /         /mod swap drop ;
  : 2*        2 * ;
  : 2/        2 / ;
  : true      -1 ;
  : false     0 ;
  : >         swap < ;
  : 0=        0 = ;
  : <>        = invert ;
  : 1+        1 + ;
  : 1-        1 - ;
  : 2+        2 + ;
  : ?dup      dup if dup then ;
  : xor       over over or rot rot and invert and 0= invert ;
  : <=        over over < rot rot = or ;
  : >=        over over > rot rot = or ;
|}

