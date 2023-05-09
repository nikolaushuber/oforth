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
  let s = ref (State.create ()) in 
  Builtins.load !s; 
  let tokens = ref [] in 
  while true do
    print_string ">> "; 
    let line = read_line () in 
    tokens := (Str.split (Str.regexp "[ \r\t]+") line); 
    try 
      while List.length !tokens > 0 do 
        let top = List.hd !tokens in 
        let top = String.lowercase_ascii top in 
        if Hashtbl.mem !s.dict top then 
          s := (Hashtbl.find !s.dict top) !s 
        else 
          s := push !s (Cell.of_string top) 
        ; 
        tokens := List.tl !tokens
      done; 
    with
    | Error.StackError    -> print_endline "--- Stack underflow"
    | Error.RStackError   -> print_endline "--- Return-Stack underflow" 
    | Failure _           -> print_endline ("--- Unknown command: " ^ List.hd !tokens) 
    | Error.ByeException  -> exit 0 
  done
