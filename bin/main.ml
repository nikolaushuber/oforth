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

let interpret tok s = 
  let tok' = String.lowercase_ascii tok in 
  apply_word s tok'

let rec loop s = 
  print_string ">> "; 
  let line = read_line () in 
  let tokens = tokenize line in 
  let next_s = List.fold_left (fun s tok -> interpret tok s) s tokens in 
  match next_s with 
  | State _ -> loop next_s
  | Error SyntaxError -> print_endline "Syntax error."; loop s
  | Error (UndefinedWord w) -> print_endline ("Undefined word: " ^ w); loop s 
  | Error StackUnderflow -> print_endline "Stack Underflow"; loop s

let run () = 
  print_endline header; 
  let s = State.create ~dict:Builtins.builtins () in 
  loop s 

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

let init_stack = 
  let doc = "Initial stack." in 
  let c = Arg.list Arg.int in 
  Arg.(value & opt c [] & info ["s";"stack"] ~doc) 

let explain input stack = 
  let tokens = tokenize input in 
  let w = List.fold_left max 6 (List.map String.length tokens) in 
  let header = (pad w "OP") ^ (pad w "Stack") in 
  print_endline header; 
  print_endline ("--    -----");
  let s = State.create ~init:stack ~dict:Builtins.builtins () in 
  print_endline ((pad w "") ^ (string_of_stack s)); 

  let rec inner tkns s = match tkns with 
    | tkn :: rest -> begin 
      print_string (pad w tkn); 
      let next_s = interpret tkn s in 
      print_endline (string_of_stack next_s); 
      match next_s with 
      | State _ -> inner rest next_s 
      | Error _ -> () 
    end 
    | [] -> () 
  in 
  inner tokens s 

let explain_cmd = Cmd.v (Cmd.info "explain") Term.(const explain $ eval_input $ init_stack ) 
  
let cmd = Cmd.group (Cmd.info "oforth") ~default [explain_cmd]

let () = Cmd.eval cmd |> exit 
