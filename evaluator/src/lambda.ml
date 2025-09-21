type term =
  | LVar of string
  | Lam of string * term
  | App of term * term

let rec substitute x s = function
  | LVar y -> if y = x then s else LVar y
  | Lam(y, body) when y = x -> Lam(y, body)
  | Lam(y, body) ->
      if List.mem y (free_variables s) then
        let y' = fresh_var () in
        let body' = substitute y (LVar y') body in
        Lam(y', substitute x s body')
      else Lam(y, substitute x s body)
  | App(f, a) -> App(substitute x s f, substitute x s a)

and free_variables = function
  | LVar x -> [x]
  | Lam(x, body) -> List.filter ((<>) x) (free_variables body)
  | App(f, a) -> free_variables f @ free_variables a

and fresh_var =
  let counter = ref 0 in
  fun () -> incr counter; "fresz" ^ string_of_int !counter

let rec reduce_once_normal = function
  | App(Lam(x, body), arg) -> Some(substitute x arg body)
  | App(f, a) ->
      (match reduce_once_normal f with
       | Some f' -> Some(App(f', a))
       | None ->
           (match reduce_once_normal a with
            | Some a' -> Some(App(f, a'))
            | None -> None))
  | Lam(x, body) ->
      (match reduce_once_normal body with
       | Some body' -> Some(Lam(x, body'))
       | None -> None)
  | LVar _ -> None

let rec reduce_once_applicative = function
  | App(Lam(x, body), arg) ->
      (match reduce_once_applicative arg with
       | Some arg' -> Some(App(Lam(x, body), arg'))
       | None -> Some(substitute x arg body))
  | App(f, a) ->
      (match reduce_once_applicative f with
       | Some f' -> Some(App(f', a))
       | None ->
           (match reduce_once_applicative a with
            | Some a' -> Some(App(f, a'))
            | None -> None))
  | Lam(x, body) ->
      (match reduce_once_applicative body with
       | Some body' -> Some(Lam(x, body'))
       | None -> None)
  | LVar _ -> None

let rec normalize ?(limit=1000) ~strategy expr =
  if limit <= 0 then None
  else
    match strategy expr with
    | Some expr' -> normalize ~limit:(limit-1) ~strategy expr'
    | None -> Some expr

let rec to_string = function
  | LVar x -> x
  | Lam(x, body) -> "λ" ^ x ^ "." ^ to_string body
  | App(f, a) ->
      let fstr = match f with Lam _ -> "(" ^ to_string f ^ ")" | _ -> to_string f in
      let astr = match a with App _ | Lam _ -> "(" ^ to_string a ^ ")" | _ -> to_string a in
      fstr ^ " " ^ astr

let omega = App(Lam("x", App(LVar "x", LVar "x")), Lam("x", App(LVar "x", LVar "x")))

let examples = [
  App(Lam("x", LVar "x"), LVar "y");                     
  App(Lam("x", Lam("y", LVar "x")), LVar "a");          
  App(App(Lam("x", Lam("y", LVar "x")), LVar "a"), LVar "b")  ;
  App(Lam("y", LVar "z"), omega)
]

let () =
  List.iter (fun e ->
    let nf_normal = normalize ~limit:1000 ~strategy:reduce_once_normal e in
    let nf_applicative = normalize ~limit:1000 ~strategy:reduce_once_applicative e in

    (match nf_normal with
     | Some nf -> Printf.printf "%s  => (normal)      %s\n" (to_string e) (to_string nf)
     | None -> Printf.printf "%s  => (normal)      Did not normalize\n" (to_string e));

    (match nf_applicative with
     | Some nf -> Printf.printf "%s  => (applicative) %s\n" (to_string e) (to_string nf)
     | None -> Printf.printf "%s  => (applicative) Did not normalize\n" (to_string e))
  ) examples