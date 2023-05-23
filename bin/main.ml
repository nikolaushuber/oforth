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

let rec main s = function 
  | top :: rest -> begin 
      let top' = String.lowercase_ascii top in 
      if List.mem_assoc top' s.dict then 
        try 
          let s' = (List.assoc top' s.dict) s in 
          main s' rest
        with 
          | Error.StackError    -> print_endline "--- Stack underflow"; main s [] 
          | Error.RStackError   -> print_endline "--- Return-Stack underflow"; main s [] 
          | Error.ByeException  -> exit 0 
      else
        try 
          let i = Cell.of_string top in 
          let s' = push s i in 
          main s' rest
        with
          Failure _ -> print_endline ("--- Unknown command: " ^ top); main s [] 
    end 
  | [] -> 
    print_string ">> "; 
    let line = read_line () in 
    let tokens = (Str.split (Str.regexp "[ \r\t]+") line) in 
    main s tokens 

let () = 
  print_endline header;
  let s = State.create () in 
  let s' = State.load_dict s Builtins.builtins in 
  main s' [] 
