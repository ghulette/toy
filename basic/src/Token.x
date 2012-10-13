{
{-# OPTIONS_GHC -w #-}
module Token (Token(..),scanTokens) where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$eol   = [\n]

tokens :-

  $eol                          ;
  $white+                       ;
  "#".*                         ;
  $digit+                       { \s -> TokenInt (read s) }
  \;                            { \s -> TokenSemicolon }
  \=                            { \s -> TokenEq }
  [\+\-\*]                      { \s -> TokenOp s }
  \(                            { \s -> TokenLParen }
  \)                            { \s -> TokenRParen }
  $alpha [$alpha $digit \_ \']* { \s -> TokenSym s }

{

data Token = TokenEq
           | TokenSemicolon
           | TokenInt Int
           | TokenSym String
           | TokenOp String
           | TokenLParen
           | TokenRParen
           deriving (Eq,Show)

scanTokens = alexScanTokens

}
