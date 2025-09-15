open Token

let is_digit c = '0' <= c && c <= '9'
let is_letter c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c = '_'

let rec tokenize s i =
  if i >= String.length s then []
  else
    match s.[i] with
    | ' ' | '\t' | '\n' -> tokenize s (i+1)
    | '(' -> LPAREN :: tokenize s (i+1)
    | ')' -> RPAREN :: tokenize s (i+1)
    | '|' -> BAR :: tokenize s (i+1)
    | '=' -> EQ :: tokenize s (i+1)
    | '-' ->
      if i+1 < String.length s && s.[i+1] = '>' then ARROW :: tokenize s (i+2)
      else failwith ("Unknown character: " ^ String.make 1 s.[i])
    | '0'..'9'  ->
      let j = ref i in
      while !j < String.length s && is_digit s.[!j] do incr j done;
      let n = int_of_string (String.sub s i (!j - i)) in
      INT n :: tokenize s !j
    | 'a'..'z' | 'A'..'Z' | '_' ->
      let j = ref i in
      while !j < String.length s && (is_letter s.[!j] || is_digit s.[!j]) do incr j done;
      let id = String.sub s i (!j - i) in
      let kw = match id with "fun" -> KW_Fun | "let" -> KW_Let | "in" -> KW_In | "match" -> KW_Match | "with" -> KW_With | _ -> IDENT id in
      kw :: tokenize s !j
    | '"' ->
      let j = ref (i+1) in
      while !j < String.length s && s.[!j] <> '"' do incr j done;
      if !j >= String.length s then failwith "Unterminated string";
      let str = String.sub s (i+1) (!j - i - 1) in
      STRING str :: tokenize s (!j + 1)
    | '\'' ->
      if i+2 >= String.length s then failwith "Invalid char literal";
      let c = s.[i+1] in
      if s.[i+2] <> '\'' then failwith "Invalid char literal";
      CHAR c :: tokenize s (i+3)
    | _ -> failwith ("Unknown character: " ^ String.make 1 s.[i])
