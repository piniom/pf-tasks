type expr = 
  | S
  | K
  | I
  | App of expr * expr
  | Var of string

let rec to_string = function
  | S -> "S"
  | K -> "K"
  | I -> "I"
  | Var v -> v
  | App (l, r) ->
    let ls = match l with App _ -> "(" ^ to_string l ^ ")" | _ -> to_string l in
    let rs = match r with App _ -> "(" ^ to_string r ^ ")" | _ -> to_string r in
    ls ^ " " ^ rs

let rec reduce_once_normal = function
  | App (App (App (S, x), y), z) -> Some (App (App (x, z), App (y, z)))
  | App (App (K, x), _y) -> Some x
  | App (I, x) -> Some x
  | App (l, r) -> 
    (match reduce_once_normal l with
     | Some l' -> Some (App (l', r))
     | None -> 
       (match reduce_once_normal r with
        | Some r' -> Some (App (l, r'))
        | None -> None))
  | _ -> None

let rec reduce_once_applicative = function
  | App (App (App (S, x), y), z) ->
      (match reduce_once_applicative z with
       | Some z' -> Some (App (App (App (S, x), y), z'))
       | None ->
           match reduce_once_applicative y with
           | Some y' -> Some (App (App (App (S, x), y'), z))
           | None ->
               match reduce_once_applicative x with
               | Some x' -> Some (App (App (App (S, x'), y), z))
               | None -> Some (App (App (x, z), App (y, z))))
  | App (I, x) ->
      (match reduce_once_applicative x with
       | Some x' -> Some (App (I, x'))
       | None -> Some x)
  | App (App (K, x), y) ->
      (match reduce_once_applicative y with
       | Some y' -> Some (App (App (K, x), y'))
       | None -> Some x)
  | App (l, r) ->
      (match reduce_once_applicative r with
       | Some r' -> Some (App (l, r'))
       | None ->
           match reduce_once_applicative l with
           | Some l' -> Some (App (l', r))
           | None -> None)
  | _ -> None

let parse_tokens toks =
  let rec parse i =
    if i >= Array.length toks then failwith "Unexpected end of input"
    else
      match toks.(i) with
      | "S" -> (S, i + 1)
      | "K" -> (K, i + 1)
      | "I" -> (I, i + 1)
      | "(" ->
          let (e, j) = parse_seq (i + 1) in
          if j >= Array.length toks || toks.(j) <> ")" then failwith "Unmatched ("
          else (e, j + 1)
      | ")" -> failwith "Unexpected )"
      | v -> (Var v, i + 1)
  and parse_seq i =
    let rec aux i acc =
      if i >= Array.length toks || toks.(i) = ")" then
        match acc with
        | [] -> failwith "Empty parentheses"
        | hd :: tl -> (List.fold_left (fun a b -> App(a, b)) hd tl, i)
      else
        let (e, j) = parse i in
        aux j (acc @ [e])
    in
    aux i []
  in
  let (expr, next) = parse_seq 0 in
  if next <> Array.length toks then failwith "Extra tokens after expression";
  expr


let normalize_normal ?(limit=1000) expr =
  let rec help n expr =
    if n <= 0 then None
    else match reduce_once_normal expr with
      | Some expr' -> help (n - 1) expr'
      | None -> Some expr
  in
  help limit expr

let normalize_applicative ?(limit=1000) expr =
  let rec help n expr =
    if n <= 0 then None
    else match reduce_once_applicative expr with
      | Some expr' -> help (n - 1) expr'
      | None -> Some expr
  in
  help limit expr


let from_string s =
  let rec tokenize acc i =
    if i >= String.length s then List.rev acc
    else
      match s.[i] with
      | ' ' -> tokenize acc (i+1)
      | '(' | ')' as c -> tokenize (Char.escaped c :: acc) (i+1)
      | _ ->
          let j = ref i in
          while !j < String.length s && not (List.mem s.[!j] [' '; '('; ')']) do
            incr j
          done;
          let token = String.sub s i (!j - i) in
          tokenize (token :: acc) !j
  in
  let toks = tokenize [] 0 in
  parse_tokens (Array.of_list toks)



let () =
  let examples = [
    "S K K";
    "S K K x";
    "K a b";
    "S S K K";
    "K I ( ( S I I ) ( S I I ) )"
  ] in
  let limit = 1000 in
  List.iter (fun s ->
    let e = from_string s in
    let nf_normal = normalize_normal ~limit e in
    let nf_applicative = normalize_applicative ~limit e in
    let nf_str_normal = match nf_normal with
      | Some expr -> to_string expr
      | None -> Printf.sprintf "Did not normalize (limit %d reached)" limit
    in
    let nf_str_applicative = match nf_applicative with
      | Some expr -> to_string expr
      | None -> Printf.sprintf "Did not normalize (limit %d reached)" limit
    in
    Printf.printf "%s  => \n normal     : %s \n applicative: %s\n"
      (to_string e)  nf_str_normal nf_str_applicative
  ) examples
