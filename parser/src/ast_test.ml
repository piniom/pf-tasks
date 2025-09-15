open Parser_lib.Ast
open Parser_lib.Lexer
open Parser_lib.Parser

let test_cases = [
  ("x", Var "x");
  ("42", Const (Int 42));
  ("'a'", Const (Char 'a'));
  ("\"hello\"", Const (String "hello"));
  ("fun x -> x", Lam ("x", Var "x"));
  ("f x", App (Var "f", Var "x"));
  ("let x = 1 in x", Let ("x", Const (Int 1), Var "x"));
  ("match x with | 0 -> 1 | n -> n", 
    Match (
      Var "x",
      [ (PConst (Int 0), Const (Int 1));
        (PVar "n", Var "n") ]
    )
  );
  ("let x = 3 in x (y z)",
    Let ("x", Const (Int 3), App (Var "x", App (Var "y", Var "z")))
  );
  ("match x with | _ -> 0", 
    Match (Var "x", [ (PVar "_", Const (Int 0)) ])
  );
]

let () =
  let parser = parse_expr () in
  List.iteri (fun i (input, expected_ast) ->
    let tokens = tokenize input 0 in
    let results = parse parser tokens in
    match results with
    | [(parsed_ast, remaining)] when remaining = [] ->
        if parsed_ast = expected_ast then
          Printf.printf "Test %d passed.\n" i
        else (
          Printf.printf "Test %d FAILED!\nInput: %s\nExpected: %s\nGot: %s\n" i input (expr_to_string expected_ast) (expr_to_string parsed_ast);
          assert false
        )
    | [(_parsed_ast, _remaining)] ->
        Printf.printf "Test %d FAILED!\nInput: %s\nUnconsumed tokens remain.\n" i input;
        assert false
    | [] ->
        Printf.printf "Test %d FAILED!\nInput: %s\nNo parse result.\n" i input;
        assert false
    | _ ->
        Printf.printf "Test %d FAILED!\nInput: %s\nAmbiguous parse.\n" i input;
        assert false
  ) test_cases
