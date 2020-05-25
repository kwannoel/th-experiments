{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Template (deriveFunctor, genCurries, genCurry)


-- | Call the template macro genCurries
-- Since argument of 3 was supplied
-- We should generate
-- curry1, curry2, curry3
$(genCurries 3)

c1 :: x1 -> x1
c1 = curry1 (\x1 -> x1)

c2 :: x1 -> x2 -> (x1, x2)
c2 = curry2 (\(x1, x2) -> (x1, x2))

c3 :: x1 -> x2 -> x3 -> (x1, x2, x3)
c3 = curry3 (\(x1, x2, x3) -> (x1, x2, x3))

-- | We can also try to call the genCurry function which generates a single tuple
$(genCurry 4)
c4 :: x1 -> x2 -> x3 -> x4 -> (x1, x2, x3, x4)
c4 = curry4 (\(x1, x2, x3, x4) -> (x1, x2, x3, x4))

-- | GenFmap
data List a = Nil
    | Cons a (List a)
data Tree a = Leaf a
    | Node (Tree a) a (Tree a)

$(deriveFunctor ''Tree)

main :: IO ()
main = putStrLn "Hello World!"
