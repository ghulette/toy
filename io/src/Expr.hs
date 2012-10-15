module Expr where

import Prelude hiding (lookup)
import Control.Monad (foldM_)
import Env

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
          | Print Expr       -- Print an expression to stdout
          deriving (Eq,Show)

-- Programs are a sequence of assignments
type Prog = [Stmt]

-- Given an environment and an expression, evaluate the expression (i.e., 
-- reduce the expression to an integer value).
evalExpr :: Env -> Expr -> Int
evalExpr env (Var x) = 
  case lookup x env of
    Just n -> n
    Nothing -> 0
evalExpr _   (Num n) = n
evalExpr env (Op op e1 e2) = (evalOp op) (evalExpr env e1) (evalExpr env e2)

-- Given an environment and a statement, return a new environment that extends 
-- the old one by performing the assignment described by the statement.
evalStmt :: Env -> Stmt -> IO Env
evalStmt env (Assign x e) = do
  let n    = evalExpr env e
  let env' = extend env x n
  putStrLn $ x ++ " = " ++ (show n)
  return env'
evalStmt env (Print e) = do
  let n = evalExpr env e
  print n
  return env

-- Given a sequence of assignment statements, evaluate them in left-to-right 
-- order, updating the environment as you go. Return the final environment.
evalProg' :: Env -> Prog -> IO ()
evalProg' _ [] = return ()
evalProg' env (s:ss) = do
  env' <- evalStmt env s
  evalProg' env' ss

-- You can implement as foldM_ evalStmt empty but, for fun...
evalProg :: Prog -> IO ()
evalProg = evalProg' empty
