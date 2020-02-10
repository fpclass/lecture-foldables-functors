--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture: Foldables                                                         --
--------------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor #-}

module Lecture where

import Prelude hiding (Foldable(..), Functor(..), ($), (<$>))

--------------------------------------------------------------------------------
-- Preliminaries

infixr 0 $
($) :: (a -> b) -> a -> b
f $ x = f x

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv x y = Just (x `div` y)

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
-- Functors

class Functor f where
    fmap :: (a -> b) -> f a -> f b

-- | Operator alias for `fmap` which associates to the left with precedence 4.
infixl 4 <$>
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

-- | Functor instance for lists.
instance Functor [] where
    fmap _ []     = []
    fmap f (x:xs) = f x : map f xs

-- | Functor instance for Maybe.
instance Functor Maybe where
    fmap _ Nothing  = Nothing
    fmap f (Just x) = Just (f x)

e0 :: Maybe Int
e0 = (+5) <$> safediv 8 4

data BinTree2 a = Leaf2 a | Node2 (BinTree2 a) (BinTree2 a)

instance Functor BinTree2 where
    fmap f (Leaf2 x)   = Leaf2 (f x)
    fmap f (Node2 l r) = Node2 (fmap f l) (fmap f r)

--------------------------------------------------------------------------------
