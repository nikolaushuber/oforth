type ast = 
  | Word of (State.t -> State.t) 
  | Block of ast list 
  | If of ast * ast 
  | Do of ast 

let rec eval ast s = match ast with 
  | Word f -> f s 
  | Block al -> List.fold_left (fun s a -> eval a s) s al 
  | If (t, e) -> 
      let cond, s' = State.pop s in 
      if cond = 0 then eval e s' else eval t s' 
  | Do body -> 
    let limit, s' = State.pop s in 
    let start, s'' = State.pop s' in 
    let idxs = if limit > start 
      then 
        List.init limit (fun x -> x + start) 
      else [] 
    in 
    List.fold_left (fun s _ -> eval body s) s'' idxs 


