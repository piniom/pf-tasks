open Parser_lib.Lexer
open Parser_lib.Parser
open Parser_lib.Ast

let tokens_to_string tokens =
  tokens |> List.map Parser_lib.Token.to_string |> String.concat " "

let () =
  let input = "(match x with | 0 -> \"zero\" | 1 -> let x = 3 in x (y z) | n -> n ) a" in
  let tokens = tokenize input 0 in
  let parser = parse_expr () in
  let result = parse parser tokens in
  match result with
  | [] -> print_endline (Printf.sprintf "Parse error(\"%s\")" (tokens_to_string tokens));
  | [(ast, t)] ->
      print_endline (expr_to_string ast);
      print_endline (Printf.sprintf "Remaining tokens: %s" (tokens_to_string t));
  | _ -> print_endline "ambiguous parse"
  
