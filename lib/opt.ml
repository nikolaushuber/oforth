type stmt = 
  | Word of string 
  | Const of int 

let prog_to_str prog = 
  let rec to_str acc = function 
    | Word w :: rest -> to_str (w :: acc) rest 
    | Const i :: rest -> to_str (string_of_int i :: acc) rest 
    | [] -> List.rev acc 
  in 
  String.concat " " (to_str [] prog) 

exception ReachedLimit 
module StringSet = Set.Make(String) 

let next max_num curr = 
  let rec incr' acc = function 
    | hd :: tl -> 
        if hd < max_num then 
          (List.rev acc) @ (hd + 1) :: tl
        else if hd == max_num then 
          (List.rev acc) @ 0 :: (incr' [] tl) 
        else
          incr' (hd :: acc) tl 
    | [] -> raise ReachedLimit 
  in
  incr' [] curr  

let step max_num curr = 
  try 
    let next' = next max_num curr in 
    Some (next', next') 
  with 
    | ReachedLimit -> None 

let make_gen max_num len = 
  let l = List.init len (fun _ -> 0) in 
  Seq.cons l (Seq.unfold (step max_num) l)

let dict_of_state (s : State.t) = match s with 
  | State s -> 
    let words, _ = List.split s.dict in 
    let dict = StringSet.of_list words in 
    let not_supported = List.map fst (Builtins.immediate @ Builtins.tools) in 
    let not_supported_dict = StringSet.of_list not_supported in 
    StringSet.elements (StringSet.diff dict not_supported_dict)
  | Error _ -> []

let dict_to_assoc_list dict = 
  List.mapi (fun index el -> (index, el), (el, index)) dict |> List.split 

let has_word = StringSet.mem
let is_int s = 
try int_of_string s |> ignore; true
with Failure _ -> false

let translate dict input = 
let tokens = Helpers.tokenize input in 
let prog = List.fold_left (fun acc word -> 
  if List.mem word dict then
    Word word :: acc 
  else if is_int word then 
    Const (int_of_string word) :: acc
  else  
  raise (Failure ("Unkown word: " ^ word))
) [] tokens in 
List.rev prog 

let apply_stmt (s : State.t) stmt = match s with 
| State _ -> begin 
  match stmt with 
    | Word w -> State.apply s (State.get_word s w) 
    | Const i -> State.push s i  
  end 
| Error _ -> s

let run_prog (s : State.t) prog = 
List.fold_left apply_stmt s prog 

let rec create_test fuel (s : State.t) prog  = 
  if fuel > 0 then begin 
    match s with 
    | State st -> 
      let stack = (Random.int 229) :: st.stack in 
      let s' = State.return {st with stack = stack} in 
      let next_s = run_prog s' prog in 
      begin match next_s with  
        | State st' -> Some (stack, st'.stack)
        | Error StackUnderflow -> create_test (fuel-1) s' prog 
        | Error _ -> failwith "Error in create test" 
      end 
    | Error _ -> failwith "Error in create test" 
  end else
    None 

let rec create_test_set num_tests fuel acc s prog = 
  if List.length acc >= num_tests then acc 
  else 
    let test = create_test fuel s prog in 
    match test with 
      | Some res -> create_test_set num_tests fuel (res :: acc) s prog  
      | None -> create_test_set num_tests fuel acc s prog 

let prepare_state (s : State.t) init = match s with 
  | State st -> State.return {st with stack = init} 
  | Error _ -> assert false 

let run_test s prog test : int list * State.t = 
  let init, expected = test in 
  let s_prep = prepare_state s init in 
  let s_final = run_prog s_prep prog in 
  expected, s_final

let extract_stack (s : State.t) = match s with 
  | State st -> Some st.stack 
  | Error _ -> None 

let make_eq_length a b = 
  let len_a = List.length a in 
  let len_b = List.length b in 
  let len = max len_a len_b in 
  let pad_a = List.init (len - len_a) (fun _ -> 0) in 
  let pad_b = List.init (len - len_b) (fun _ -> 0) in 
  a @ pad_a, b @ pad_b 

let compare_result expected gotten = 
  let exp, got = make_eq_length expected gotten in 
  let diff = List.map2 (-) exp got in 
  let abs_diff = List.map abs diff in 
  List.fold_left (+) 0 abs_diff  

let eq s testset rewrite = 
  let results = List.map (run_test s rewrite) testset in 
  let diff = List.map (fun (exp, got) -> 
    match extract_stack got with 
    | Some l -> compare_result exp l 
    | None -> Int.max_int 
  ) results in 
  if List.fold_left (+) 0 diff == 0 then true else false 

let find_opt_of_len max_num trans_f eq_f printer len = 
  let next_candidate = make_gen max_num len in 
  Seq.fold_left (fun _ cand -> 
    if eq_f (trans_f cand) then printer cand 
  ) () next_candidate  

let rec opt max_num trans_f eq_f printer len idx = 
  if idx >= len then () else 
  find_opt_of_len max_num trans_f eq_f printer idx; 
  opt max_num trans_f eq_f printer len (idx + 1)

let f ?(num_tests = 30) ?(fuel = 100) ?(max_int = 10) prog_str = 
  let s = State.create ~dict:Builtins.builtins () |> State.load_core in 
  let dict = dict_of_state s in 
  let int_to_w, _ = dict_to_assoc_list dict in 
  let dict_len = List.length dict in 
  let trans_f = List.map (fun x -> match List.assoc_opt x int_to_w with 
    | Some w -> Word w 
    | None -> Const (x - dict_len) 
  ) in 
  let prog_printer = fun x -> print_endline (trans_f x |> prog_to_str) in 
  let prog = translate dict prog_str in 
  let testset = create_test_set num_tests fuel [] s prog in 
  let eq_f = eq s testset in 
  opt (dict_len + max_int) trans_f eq_f prog_printer (List.length prog) 1 

  
