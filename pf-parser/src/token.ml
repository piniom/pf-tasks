type token =
  | KW_Fun | KW_Let | KW_In | KW_Match | KW_With | BAR 
  | ARROW | EQ
  | LPAREN | RPAREN
  | INT of int
  | CHAR of char
  | STRING of string
  | IDENT of string

let to_string t =
  match t with
  | KW_Fun -> "fun"
  | KW_Let -> "let"
  | KW_In -> "in"
  | KW_Match -> "match"
  | KW_With -> "with"
  | BAR -> "|"
  | ARROW -> "=>"
  | EQ -> "="
  | LPAREN -> "("
  | RPAREN -> ")"
  | INT n -> Printf.sprintf "INT(%d)" n
  | CHAR c -> Printf.sprintf "CHAR('%c')" c
  | STRING s -> Printf.sprintf "STRING(\"%s\")" s
  | IDENT s -> Printf.sprintf "IDENT(%s)" s