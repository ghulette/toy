module MyFunctor where

class MyFunctor f where
  myfmap :: (a -> b) -> f a -> f b

-- Box: a very simple container datatype

data Box a = Box a deriving (Eq,Show)

instance MyFunctor Box where
  myfmap f (Box x) = Box (f x)


-- Option: same thing as Maybe

data Option a = Some a 
              | None
              deriving (Eq,Show)

instance MyFunctor Option where
  myfmap f None = undefined
  myfmap f (Some x) = undefined


-- MyList: our own list datatype

data MyList a = Nil 
              | Cons a (MyList a)
              deriving (Eq,Show)

instance MyFunctor MyList where
  myfmap = undefined

demo1 :: IO ()
demo1 = do
  print $ myfmap (+1) (Box 5)
  print $ myfmap (even) (Box 5)
  print $ myfmap (\x -> if x > 10 then "Hello" else "Bye") (Box 5)
  print $ myfmap (+1) (Some 5)
  print $ myfmap (+1) (None)
  print $ myfmap (+1) (Cons 1 (Cons 2 (Cons 3 Nil)))
