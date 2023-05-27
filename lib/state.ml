type error = 
  | StackUnderflow 
  | UndefinedWord of string 

type state = {
  stack : int list; 
  dict : (string * (t -> t)) list; 
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
    dict; 
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

let apply_word s w = match s with 
  | State st -> 
    if has_word s w then 
      (List.assoc w st.dict) s 
    else 
      Error (UndefinedWord w)
  | Error _ -> s 

let string_of_stack s = match s with 
  | State s -> "[ " ^ String.concat ", " (List.map string_of_int s.stack) ^ " ]"
  | Error StackUnderflow -> "Stack Underflow." 
  | Error (UndefinedWord w) -> "Undefined word: " ^ w 
