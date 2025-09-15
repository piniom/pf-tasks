type const =
  | Int of int
  | Char of char
  | String of string

type pattern =
  | PVar of string
  | PConst of const
  | PWildcard

type expr =
  | Var of string
  | Const of const
  | Lam of string * expr
  | App of expr * expr
  | Let of string * expr * expr
  | Match of expr * (pattern * expr) list


  let const_to_string = function
  | Int i -> string_of_int i
  | Char c -> "'" ^ String.make 1 c ^ "'"
  | String s -> "\"" ^ s ^ "\""

let pattern_to_string = function
  | PVar v -> v
  | PConst c -> const_to_string c
  | PWildcard -> "_"

let indent_lines n s =
  s
  |> String.split_on_char '\n'
  |> List.map (fun line -> (String.make n '\t') ^ line)
  |> String.concat "\n"

let rec expr_to_string ?(indent=0) = function
  | Var v -> v
  | Const c -> const_to_string c
  | Lam (x, e) ->
      let body = expr_to_string ~indent:(indent+1) e in
      Printf.sprintf "%sfun %s ->\n%s" (String.make indent '\t') x (indent_lines (indent+1) body)
  | App (e1, e2) ->
      Printf.sprintf "%s(%s) (%s)" (String.make indent '\t') (expr_to_string e1) (expr_to_string e2)
  | Let (x, e1, e2) ->
      let e1s = expr_to_string ~indent:(indent+1) e1 |> indent_lines (indent+1) in
      let e2s = expr_to_string ~indent e2 in
      Printf.sprintf "%slet %s =\n%s\n%sin %s" (String.make indent '\t') x e1s (String.make indent '\t') e2s
  | Match (e, branches) ->
      let e_s = expr_to_string ~indent e in
      let branches_s =
        branches
        |> List.map (fun (p, e) ->
             let e_s = expr_to_string ~indent:(indent) e |> indent_lines (indent + 1) in
             Printf.sprintf "%s| %s ->\n%s" (String.make (indent) '\t') (pattern_to_string p) e_s)
        |> String.concat "\n"
      in
      Printf.sprintf "%smatch %s with\n%s" (String.make indent '\t') e_s branches_s

