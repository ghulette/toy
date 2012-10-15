{
module Parser (parse) where

import Token
import Expr
}

%name prog
%tokentype { Token }
%error { parseError }

%token
    int   { TokenInt $$ }
    var   { TokenSym $$ }
    print { TokenPrint }
    '='   { TokenEq }
    ';'   { TokenSemicolon }
    op    { TokenOp $$ }
    '('   { TokenLParen }
    ')'   { TokenRParen }

%left '+'
%left '-'
%left '*'

%%

Prog :                                   { [] }
     | Stmt Prog                         { $1 : $2 }

Stmt : var '=' Expr ';'                  { Assign $1 $3 }
     | print Expr ';'                    { Print $2 }

Expr : Expr op Expr                      { Op (encodeOp $2) $1 $3 }
     | '(' Expr ')'                      { $2 }
     | int                               { Num $1 }
     | var                               { Var $1 }

{

encodeOp :: String -> Op
encodeOp "+" = Add
encodeOp "-" = Sub
encodeOp "*" = Mult

parseError :: [Token] -> a
parseError _ = error "Parse error"

parse :: String -> Prog
parse = prog . scanTokens

}
