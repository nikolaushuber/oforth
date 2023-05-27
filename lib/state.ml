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

let add_word s w f = match s with 
  | State s -> return {s with dict = (w, f) :: s.dict} 
  | Error _ -> s 

let rec ast0_to_ast s (ast0 : Ast0.t) : ast = match ast0 with 
  | Block bl -> Block (List.map (ast0_to_ast s) bl) 
  | If (e, t) -> If (ast0_to_ast s e, ast0_to_ast s t) 
  | Do body -> Do (ast0_to_ast s body)
  | Word w -> 
    if has_word s w then 
      let f = get_word s w in 
      Word f 
    else
      raise (UnknownWord w)

let compile s input = match input with 
  | name :: rest -> 
    let ast0 = to_ast0 rest [] in 
    name, ast0_to_ast s ast0 
  | _ -> raise SyntaxError 


let rec eval ast s : t = match ast with  
  | Word w -> apply s w 
  | Block al -> List.fold_left (fun s a -> eval a s) s al 
  | If (t, e) -> 
      let cond, s' = pop s in 
      if cond = 0 then eval e s' else eval t s' 
  | Do body -> 
    let limit, s' = pop s in 
    let start, s'' = pop s' in 
    let idxs = if limit > start 
      then 
        List.init limit (fun x -> x + start) 
      else [] 
    in 
    List.fold_left (fun s _ -> eval body s) s'' idxs 

and apply s = function 
  | Primitive f -> f s 
  | UserDefined ast -> eval ast s 

let apply_word s w = match s with 
  | State st -> 
    if has_word s w then 
      apply s (List.assoc w st.dict)
    else 
      Error (UndefinedWord w)
  | Error _ -> s 

let start_compiling s = match s with 
  | Error _ -> s 
  | State st -> 
      if st.compiling then Error SyntaxError 
      else return {st with compiling = true} 

let end_compiling s = match s with 
  | Error _ -> s 
  | State st ->
      if st.compiling then
        let name, ast = compile s st.compile_buffer in 
        add_word s name (UserDefined ast) 
      else 
        Error SyntaxError 

let string_of_stack s = match s with 
  | State s -> "[ " ^ String.concat ", " (List.map string_of_int s.stack) ^ " ]"
  | Error StackUnderflow -> "Stack Underflow." 
  | Error (UndefinedWord w) -> "Undefined word: " ^ w 
  | _ -> assert false 


