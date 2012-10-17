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
evalExpr = undefined -- You can just copy this from the last exercise.

-- Given an environment and a statement, return a new environment that extends 
-- the old one by performing the assignment described by the statement.
evalStmt :: Env -> Stmt -> IO Env
evalStmt = undefined

-- Given a sequence of statements, evaluate them in left-to-right order. We
-- don't care about the final Env, we just want the side effects.
evalProg :: Prog -> IO ()
evalProg = undefined
