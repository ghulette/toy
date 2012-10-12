module Expr where

import Prelude hiding (lookup)

-- Identifiers
type Id = String

-- Binary operators
data Op = Add | Sub | Mult deriving (Eq,Show)

-- Given one of our operators, return the corresponding arithmetic function.
evalOp :: Op -> (Int -> Int -> Int)
evalOp Add  = (+)
evalOp Sub  = (-)
evalOp Mult = (*)

-- Expressions
data Expr = Var Id           -- Reference to a variable
          | Num Int          -- Numeric constant
          | Op Op Expr Expr  -- Arithmetic operation
          deriving (Eq,Show)

-- Statements
data Stmt = Assign Id Expr   -- Assignment to a variable
          deriving (Eq,Show)

-- Programs are a sequence of assignments
type Prog = [Stmt]

-- An environment is used to keep track of the current value of each variable.
type Env = [(Id,Int)]

-- Empty environment (no variables assigned)
empty :: Env
empty = []

-- Given an environment and a variable name, return the value that is assigned
-- to that name in the environment. If the variable name is unbound, return
-- Nothing.
lookup :: Id -> Env -> Maybe Int
lookup _ []                    = Nothing
lookup k ((x,n):_) | k == x    = Just n
lookup k (_:env)   | otherwise = lookup k env

-- "Update" an environment with a new binding.
extend :: Env -> Id -> Int -> Env
extend env k n = (k,n):env

-- Given an environment and an expression, evaluate the expression (i.e., 
-- reduce the expression to an integer value).
evalExpr :: Env -> Expr -> Int
evalExpr _ (Num n) = n
evalExpr env (Var x) = 
  case lookup x env of
    Just n -> n
    Nothing -> 0
evalExpr env (Op o e1 e2) = (evalExpr env e1) `op` (evalExpr env e2)
  where op = evalOp o

-- Given an environment and a statement, return a new environment that extends 
-- the old one by performing the assignment described by the statement.
evalStmt :: Env -> Stmt -> Env
evalStmt env (Assign x e) = insert env x n
  where n = evalExpr env e

-- Given a sequence of assignment statements, evaluate them in left-to-right 
-- order, updating the environment as you go. Return the final environment.
evalProg :: Prog -> Env
evalProg p = foldl evalStmt empty p

-- Evaluate the program as before, but instead of returning the final 
-- environment, return the value bound by the final assignment.
evalProgFinal :: Prog -> Int
evalProgFinal p = snd (head (evalProg p))
