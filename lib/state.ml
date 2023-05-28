open Ast0

exception SyntaxError 
exception UnknownWord of string 

type error = 
  | StackUnderflow 
  | UndefinedWord of string 
  | SyntaxError 

type word = 
  | Primitive of (t -> t) 
  | UserDefined of ast 

and ast = 
  | Word of word
  | Const of int 
  | Block of ast list 
  | If of ast * ast 
  | Do of ast 

and state = {
  stack : int list; 
  dict : (string * word) list; 
  compile_buffer : string list; 
  compiling : bool; 
}

and t = 
  | State of state
  | Error of error 

let ( let* ) o f = match o with 
  | State s -> f s 
  | Error _ -> o 

let return s = State s 

let create ?(init = []) ?(dict = []) () = 
  State {
    stack = List.rev init;
    dict = List.map (fun (name, f) -> name, Primitive f) dict; 
    compile_buffer = []; 
    compiling = false; 
  }

let is_int s = 
  try int_of_string s |> ignore; true
  with Failure _ -> false

let pop s = match s with 
  | State s -> begin match s.stack with 
    | i :: il -> i, return {s with stack = il}
    | _ -> 0, Error StackUnderflow 
  end
  | Error _ -> 0, s 

let push s i = match s with 
  | State s -> return {s with stack = i :: s.stack}
  | Error _ -> s 

let has_word s w = match s with 
  | State s -> List.mem_assoc w s.dict 
  | Error _ -> false 

let get_word s w = match s with 
  | State s -> List.assoc w s.dict 
  | Error _ -> assert false 

let add_word s w f = {s with dict = (w, f) :: s.dict} 


let rec ast0_to_ast s (ast0 : Ast0.t) : ast = match ast0 with 
  | Block bl -> Block (List.map (ast0_to_ast s) bl) 
  | If (e, t) -> If (ast0_to_ast s e, ast0_to_ast s t) 
  | Do body -> Do (ast0_to_ast s body)
  | Word w -> 
    if has_word s w then 
      let f = get_word s w in 
      Word f 
    else if is_int w then 
      Const (int_of_string w) 
    else
      raise (UnknownWord w)

let compile s input = match input with 
  | name :: rest -> 
    let ast0 = to_ast0 rest [] in 
    name, ast0_to_ast s ast0 
  | _ -> raise SyntaxError 

let rec eval ast s : t = match ast with  
  | Word w -> apply s w 
  | Const i -> push s i 
  | Block al -> List.fold_left (fun s a -> eval a s) s al 
  | If (t, e) -> 
      let cond, s' = pop s in 
      if cond = 0 then eval e s' else eval t s' 
  | Do body -> 
    let start, s' = pop s in 
    let limit, s'' = pop s' in 
    let idxs = if limit > start 
      then 
        List.init limit (fun x -> x + start) 
      else [] 
    in 
    List.fold_left (fun s _ -> eval body s) s'' idxs 

and apply s = function 
  | Primitive f -> f s 
  | UserDefined ast -> eval ast s 

let start_compiling s = match s with 
  | Error _ -> s 
  | State st ->  
      if st.compiling then Error SyntaxError 
      else return {st with compiling = true} 

let end_compiling s = match s with 
  | Error _ -> s 
  | State st ->
      if st.compiling then begin 
        try
          let name, ast = compile s (List.rev st.compile_buffer) in 
          let s = add_word st name (UserDefined ast) in 
          return {s with compiling = false; compile_buffer = []}
        with 
          | SyntaxError -> Error SyntaxError 
          | UnknownWord w -> Error (UndefinedWord w) 
      end else Error SyntaxError 

let apply_word s w = match s with 
  | State st -> if st.compiling then begin 
      if String.equal w ";" then 
        end_compiling s 
      else 
        State {st with compile_buffer = w :: st.compile_buffer} 
    end else
      if has_word s w then 
        apply s (List.assoc w st.dict)
      else if is_int w then 
        push s (int_of_string w)
      else 
        Error (UndefinedWord w)
  | Error _ -> s 

let string_of_stack s = match s with 
  | State s -> "[ " ^ String.concat ", " (List.map string_of_int (List.rev s.stack)) ^ " ]"
  | Error StackUnderflow -> "Stack Underflow." 
  | Error (UndefinedWord w) -> "Undefined word: " ^ w 
  | _ -> assert false 

let interpret tok s = 
  let tok' = String.lowercase_ascii tok in 
  apply_word s tok'

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

let load_core s = 
  let tokens = Helpers.tokenize core in 
  try 
    List.fold_left (fun s x -> interpret x s) s tokens
  with
    | _ -> Error SyntaxError 
