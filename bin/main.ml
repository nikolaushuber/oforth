open Cmdliner 
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

let tokenize = Str.split (Str.regexp "[ \r\t]+")

let interpret s tok = 
  let tok' = String.lowercase_ascii tok in 
  if List.mem_assoc tok' s.dict then 
      (List.assoc tok' s.dict) s  
  else
      let i = Cell.of_string tok' in 
      push s i 
    
let rec main s = function 
  | top :: rest -> begin 
    try
      let s' = interpret s top in 
      main s' rest
    with 
      | Error.StackError    -> print_endline "--- Stack underflow"; main s [] 
      | Error.RStackError   -> print_endline "--- Return-Stack underflow"; main s [] 
      | Error.ByeException  -> exit 0 
      | Failure _ -> print_endline ("--- Unknown command: " ^ top); main s []   
    end 
  | [] -> print_string ">> "; read_line () |> tokenize |> main s 

let run () = 
  print_endline header;
  let s = State.create () in 
  let s' = State.load_dict s Builtins.builtins in 
  main s' [] 

let default = Term.(const run $ const ()) 

let pad len s = 
  let s_len = String.length s in 
  if s_len == len then 
    s 
  else
    s ^ (String.make (len - s_len) ' ')

let eval_input = 
  let doc = "Input to evaluate." in 
  Arg.(value & pos 0 string "" & info [] ~doc)

let explain input = 
  let tokens = tokenize input in 
  let w = List.fold_left max 6 (List.map String.length tokens) in 
  let header = (pad w "OP") ^ (pad w "Stack") in 
  print_endline header; 
  print_endline ("--    -----");
  let s = State.create () in 
  let s' = State.load_dict s Builtins.builtins in 
  try 
    ignore (List.fold_left (fun state tok ->
      print_string (pad w tok); 
      let next = interpret state tok in 
      print_endline (string_of_stack next); 
      next
    ) s' tokens)
  with 
      | Error.StackError    -> print_endline "Stack underflow"
      | Error.RStackError   -> print_endline "Return-Stack underflow"
      | Error.ByeException  -> print_endline "" 
      | Failure _ -> print_endline ("Unknown command")
  
let explain_cmd = Cmd.v (Cmd.info "explain") Term.(const explain $ eval_input) 
  
let cmd = Cmd.group (Cmd.info "oforth") ~default [explain_cmd]

let () = Cmd.eval cmd |> exit 
