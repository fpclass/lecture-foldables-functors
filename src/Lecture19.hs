--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture 19: Foldables                                                      --
--------------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor #-}

module Lecture19 where

import Prelude hiding (Foldable(..))

import System.Random

--------------------------------------------------------------------------------
-- Foldables

data BinTree a = Leaf | Node (BinTree a) a (BinTree a)
    deriving Show

-- an abbreviated Foldable type class, the real Foldable class in Prelude has
-- many more methods
class Foldable t where
    foldr :: (a -> b -> b) -> b -> t a -> b

    sum :: Num a => t a -> a
    sum = foldr (+) 0

    length :: t a -> Int
    length = foldr (\_ r -> 1+r) 0

instance Foldable [] where
    foldr _ z []     = z
    foldr f z (x:xs) = f x (foldr f z xs)

instance Foldable BinTree where
    foldr _ z Leaf         = z
    foldr f z (Node l x r) = f x (foldr f (foldr f z r) l)

average :: Foldable f => f Int -> Int
average xs = sum xs `div` length xs

--------------------------------------------------------------------------------
-- State type

data State s a = St { runState :: s -> (a,s) }
    deriving Functor -- lab exercise!

instance Applicative (State s) where
   pure x = St (\s -> (x,s))

   St mf <*> St mx = St (\s -> let (f,s') = mf s
                                   (x,s'') = mx s'
                               in (f x,s''))

fresh :: State Int Int
fresh = St (\s -> (s,s+1))

ex0 :: (Int, Int)
ex0 = runState fresh 5

ex1 :: (Int, Int)
ex1 = runState ((+) <$> fresh <*> fresh) 5

ex2 :: (Int, Int)
ex2 = runState (fresh *> fresh *> fresh) 5

ex3 :: (Int, Int)
ex3 = runState ((+) <$> (fresh *> fresh) <*> fresh) 5

--------------------------------------------------------------------------------
-- Labelling trees

calculateTotal :: Int -> State Int Int
calculateTotal x = St (\s -> (s+x,s+x))

labelTree :: BinTree Int -> State Int (BinTree Int)
labelTree Leaf         = pure Leaf
labelTree (Node l x r) = Node <$> labelTree l
                              <*> calculateTotal x
                              <*> labelTree r

--------------------------------------------------------------------------------
-- Additional examples

randomNumber :: State StdGen Int
randomNumber = St random

ex4 :: Int -> Int
ex4 = fst . runState randomNumber . mkStdGen

--------------------------------------------------------------------------------
