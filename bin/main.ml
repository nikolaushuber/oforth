open Oforth
open State

let version = "0.1"

let header = 
"       ,------.               ,--.  ,--.      
 ,---. |  .---',---. ,--.--.,-'  '-.|  ,---.  
| .-. ||  `--,| .-. ||  .--''-.  .-'|  .-.  | 
' '-' '|  |`  ' '-' '|  |     |  |  |  | |  | 
 `---' `--'    `---' `--'     `--'  `--' `--'                                              
Version: " ^ version ^ "  License: EUPL
Type \"bye\" to leave the interpreter. 
"

let () = 
  print_endline header;

  let dict = Dictionary.create () in 
  Dictionary.load_builtins dict; 
  let curr_state = ref (State.create ()) in 
  let tokens = ref [] in 
  ignore !curr_state.rstack; 
  while true do
    print_string ">> "; 
    let line = read_line () in 
    tokens := (Str.split (Str.regexp "[ \r\t]+") line); 
    try 
      while List.length !tokens > 0 do 
        let top = List.hd !tokens in 
        let top = String.lowercase_ascii top in 
        if Hashtbl.mem dict top then 
          curr_state := (Hashtbl.find dict top) !curr_state 
        else 
          curr_state := push !curr_state (int_of_string top) 
        ; 
        tokens := List.tl !tokens
      done; 
    with
    | Error.StackError    -> print_endline "--- Stack underflow"
    | Error.RStackError   -> print_endline "--- Return-Stack underflow" 
    | Failure _           -> print_endline ("--- Unknown command: " ^ List.hd !tokens) 
    | Error.ByeException  -> exit 0 
  done
