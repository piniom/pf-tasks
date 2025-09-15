open Token
open Ast
open List

exception SyntaxError of string

type tokens = token list

type 'a parser = Parser of (tokens -> ('a * tokens) list)

let parse (p : 'a parser) (ts : tokens) : ('a * tokens) list =
  match p with
  | Parser fp -> fp ts

let return (x : 'a) : 'a parser =
  Parser (fun input -> [(x, input)])

let map (p : 'a parser) (f : 'a -> 'b) : 'b parser =
  Parser (fun input -> let lst = parse p input in List.map (fun (a, rest) -> (f a, rest)) lst)

let bind (Parser p) f =
  Parser (fun ts ->
    concat_map (fun (a, rest) ->
      let Parser p' = f a in
      p' rest
    ) (p ts)
  )

let (>>=) = bind

let alternative (parsers : 'a parser list) : 'a parser =
    Parser (fun ts -> List.map (fun (Parser p) -> p ts) parsers |> List.concat)

let join_parsers (Parser p) (Parser q) fJoin =
  Parser (fun input -> 
    concat_map (fun (a, rest) -> List.map (fun (b, restt) -> (fJoin a b, restt)) (q rest)) (p input))

let rec repeated (p: 'a parser) : 'a list parser =
    Parser (fun input -> 
    let lst = parse p input in
    if (List.length lst == 0) then [[], input] else
    let recParser = repeated p in
    List.concat_map (fun (a, rest) -> let further = (parse recParser rest) in List.map (fun (atail, resttail) -> (a::atail, resttail)) (if List.length further == 0 then [([], rest)] else further)) lst
)

let filter_pred (pred : (token -> bool)) : token parser = 
    Parser (fun ts -> match ts with
    | [] -> []
    | t::rest -> if (pred t) then [(t, rest)] else [])

let filter_map (p : 'a parser) (f : 'a -> 'b option) : 'b parser =
  Parser (fun input ->
    parse p input
    |> List.filter_map (fun (a, rest) ->
         match f a with
         | Some b -> Some (b, rest)
         | None -> None))

let token_parser (test : token -> 'a option) (wrap : 'a -> 'b) : 'b parser =
  filter_map (filter_pred (fun t -> Option.is_some (test t)))
             (fun t -> match test t with Some x -> Some (wrap x) | None -> None)
let int_parser     = token_parser (function INT n   -> Some n | _ -> None) (fun n -> Int n)
let string_parser  = token_parser (function STRING s-> Some s | _ -> None) (fun s -> String s)
let char_parser    = token_parser (function CHAR c  -> Some c | _ -> None) (fun c -> Char c)

let const_parser : const parser = 
  alternative [
    int_parser;
    string_parser;
    char_parser
  ]

let var_parser = token_parser (function IDENT s -> Some s | _ -> None) (fun s -> s)


let pattern_parser : pattern parser =
  alternative [
    map var_parser (fun x -> PVar x);
    map const_parser (fun c -> PConst c);
  ]

let rec parse_atom () : expr parser =
  alternative [
    map var_parser (fun x -> Var x);
    map const_parser (fun c -> Const c);
    (
      filter_pred (function LPAREN -> true | _ -> false) >>= fun _ ->
      parse_expr () >>= fun e ->
      filter_pred (function RPAREN -> true | _ -> false) >>= fun _ ->
      return e
    )
  ]

and parse_app () : expr parser =
  parse_atom () >>= fun f ->
  repeated (parse_atom ()) >>= fun args ->
  return (List.fold_left (fun acc x -> App(acc, x)) f args)

and parse_expr () : expr parser =
  alternative [
    (
      filter_pred (function KW_Fun -> true | _ -> false) >>= fun _ ->
      var_parser >>= fun x ->
      filter_pred (function ARROW -> true | _ -> false) >>= fun _ ->
      parse_expr () >>= fun body ->
      return (Lam (x, body))
    );
    (
      filter_pred (function KW_Let -> true | _ -> false) >>= fun _ ->
      var_parser >>= fun x ->
      filter_pred (function EQ -> true | _ -> false) >>= fun _ ->
      parse_expr () >>= fun e1 ->
      filter_pred (function KW_In -> true | _ -> false) >>= fun _ ->
      parse_expr () >>= fun e2 ->
      return (Let (x, e1, e2))
    );
    (
      filter_pred (function KW_Match -> true | _ -> false) >>= fun _ ->
      parse_expr () >>= fun e ->
      filter_pred (function KW_With -> true | _ -> false) >>= fun _ ->
      repeated (parse_branches ()) >>= fun branches ->
      return (Match (e, branches))
    );
    parse_app ()
  ]

and parse_branches (_ : unit) : (pattern * expr) parser =
  filter_pred (function BAR -> true | _ -> false) >>= fun _ ->
  pattern_parser >>= fun p ->
  filter_pred (function ARROW -> true | _ -> false) >>= fun _ ->
  parse_expr () >>= fun body ->
  return (p, body)