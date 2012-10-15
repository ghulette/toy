module Env where

import Prelude hiding (lookup)

-- Identifiers
type Id = String

-- An environment is used to keep track of the current value of each variable.
type Env = Id -> Maybe Int

-- Empty environment (no variables assigned)
empty :: Env
empty = const Nothing

-- "Update" an environment with a new binding.
extend :: Env -> Id -> Int -> Env
extend env k n = \x -> if x == k then Just n else env x

-- Given an environment and a variable name, return the value that is assigned
-- to that name in the environment. If the variable name is unbound, return
-- Nothing.
lookup :: Id -> Env -> Maybe Int
lookup k env = env k
