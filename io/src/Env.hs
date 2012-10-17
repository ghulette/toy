module Env where

import Prelude hiding (lookup)

-- Identifiers
type Id = String

-- An environment is used to keep track of the current value of each variable.
type Env = Id -> Maybe Int

-- Empty environment (no variables assigned)
empty :: Env
empty = undefined

-- "Update" an environment with a new binding.
extend :: Env -> Id -> Int -> Env
extend = undefined

-- Given an environment and a variable name, return the value that is assigned
-- to that name in the environment. If the variable name is unbound, return
-- Nothing.
lookup :: Id -> Env -> Maybe Int
lookup = undefined
