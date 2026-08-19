
module Sexp = struct type t = LPAREN | RPAREN | ATOM [@@deriving ord, show] end
module Csv = struct type t = CRLF | COMMA | FIELD [@@deriving ord, show] end
module Json = struct
  type t = COMMA | COLON | STRING | LBRACE | RBRACE
         | LBRACKET | RBRACKET | DECIMAL | NULL | TRUE | FALSE [@@deriving ord, show]
end
module Intexp = struct
  type t = LET | ID | IN | INT | TIMES | PLUS | MINUS
         | EQUAL | THEN | ELSE | IF | LPAREN | RPAREN [@@deriving ord, show]
end
module Pgn = struct
  type t = COORDINATE | CASTLE | STAR | INT | INTDOT | MINUS | SLASH
         | STRING | LBRACKET | RBRACKET | TAG [@@deriving ord, show]
end
module Ppm = struct type t = INT | P3 [@@deriving ord, show] end
