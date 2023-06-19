type stmt = 
  | Word of string 
  | Const of int 
  | Unused 

type prog = stmt list 

let prog_to_str prog = 
  let rec to_str acc = function 
    | Word w :: rest -> to_str (w :: acc) rest 
    | Const i :: rest -> to_str (string_of_int i :: acc) rest 
    | Unused :: rest -> to_str acc rest 
    | [] -> List.rev acc 
  in 
  String.concat " " (to_str [] prog) 

module StringSet = Set.Make(String) 

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
  
let dict_of_state (s : State.t) = match s with 
  | State s -> 
    let words, _ = List.split s.dict in 
    StringSet.of_list words
  | Error _ -> StringSet.empty

let perf target rewrite = 
  let cost prog = List.fold_left (fun acc stmt -> match stmt with 
    | Word _ | Const _ -> acc + 1 
    | Unused -> acc 
  ) 0 prog 
  in 
  (cost rewrite) - (cost target)

let apply_stmt (s : State.t) stmt = match s with 
  | State _ -> begin 
    match stmt with 
      | Word w -> State.apply s (State.get_word s w) 
      | Const i -> State.push s i  
      | Unused -> s 
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
  List.fold_left (+) 0 diff 

module Move = struct 
  type t = 
    | Swap 
    | ChangeInstr 
    | DeleteInstr 
    | IntroConst 

  (* This should probably be parameterized by propabilities for each move *)
  let get () = match Random.int 3 with 
    | 0 -> Swap 
    | 1 -> ChangeInstr 
    | 2 -> DeleteInstr 
    | _ -> IntroConst  
end 

let set_nth idx el l = 
  List.mapi (fun i elem -> if i == idx then el else elem) l 

let mutate dict_list prog = 
  let prog_len = List.length prog in 

  match Move.get () with 
  | Swap -> 
    let idx1 = Random.full_int prog_len in 
    let idx2 = Random.full_int prog_len in 
    set_nth idx1 (List.nth prog idx2) prog 
    |> set_nth idx2 (List.nth prog idx1) 

  | ChangeInstr -> 
    let idx = Random.full_int prog_len in 
    let num_words = List.length dict_list in 
    let instr = Word (List.nth dict_list (Random.full_int num_words)) in 
    set_nth idx instr prog 

  | DeleteInstr -> 
    let idx = Random.full_int prog_len in 
    set_nth idx Unused prog  

  | IntroConst -> 
    let const = Random.full_int Int.max_int in 
    let idx = Random.full_int prog_len in 
    set_nth idx (Const const) prog 

let legal_dict dict = 
  let not_supported = List.map fst (Builtins.immediate @ Builtins.tools) in 
  let not_supported_dict = StringSet.of_list not_supported in 
  StringSet.diff dict not_supported_dict

let cost s testset target rewrite = 
  let eq' = eq s testset rewrite in 
  let perf' = perf target rewrite in 
  float_of_int (eq' + perf')

let step mutate_f cost_f tabu_list curr = 
  let rewrite = mutate_f curr in 
  let cost_rewrite = cost_f rewrite in 
  let cost_curr = cost_f curr in 
  if List.mem rewrite tabu_list then 
    curr, tabu_list 
  else
  if cost_rewrite < cost_curr then 
    rewrite, rewrite :: (List.tl tabu_list)  
  else
    curr, tabu_list 
  
let rec opt mutate_f cost_f tabu_list curr = 
  let rewrite, tabu' = step mutate_f cost_f tabu_list curr in 
  print_endline (prog_to_str rewrite);
  opt mutate_f cost_f tabu' rewrite 

let f ?(num_tests = 30) ?(fuel = 100) ?(tabu_len = 10) prog_str = 
  let s = State.create ~dict:Builtins.builtins () |> State.load_core in 
  let dict = dict_of_state s |> legal_dict |> StringSet.elements in 
  let prog = translate dict prog_str in 
  let mutator = mutate dict in 
  let testset = create_test_set num_tests fuel [] s prog in 
  let cost_f = cost s testset prog in 
  let tabu_list = List.init tabu_len (fun _ -> prog) in 
  opt mutator cost_f tabu_list prog 
