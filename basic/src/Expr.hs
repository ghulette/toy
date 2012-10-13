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
type Env = Id -> Maybe Int

-- Empty environment (no variables assigned)
empty :: Env
empty = const Nothing

-- "Update" an environment with a new binding.
extend :: Env -> Id -> Int -> Env
extend env k n = \x -> if x == k then Just n else env k

-- Given an environment and a variable name, return the value that is assigned
-- to that name in the environment. If the variable name is unbound, return
-- Nothing.
lookup :: Id -> Env -> Maybe Int
lookup k env = env k

-- Given an environment and an expression, evaluate the expression (i.e., 
-- reduce the expression to an integer value).
evalExpr :: Env -> Expr -> Int
evalExpr = undefined

-- Given an environment and a statement, return a new environment that extends 
-- the old one by performing the assignment described by the statement.
evalStmt :: Env -> Stmt -> Env
evalStmt = undefined

-- Given a sequence of assignment statements, evaluate them in left-to-right 
-- order, updating the environment as you go. Return the final environment.
evalProg :: Prog -> Env
evalProg = undefined

-- Evaluate the program as before, but instead of returning the final 
-- environment, return the value bound by the final assignment.
evalProgFinal :: Prog -> Int
evalProgFinal = undefined
