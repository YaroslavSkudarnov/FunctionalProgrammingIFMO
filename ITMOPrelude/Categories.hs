{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Categories where

import ITMOPrelude.List
import ITMOPrelude.Primitive
import ITMOPrelude.Tree

class Functor f where  
    fmap :: (a -> b) -> f a -> f b

class Monad m where  
    (>>=) :: m a -> (a -> m b) -> m b
    (>>) :: m a -> m b -> m b
    return :: a -> m a

class Category cat where
    id :: cat a a
    (<.>) :: cat b c -> cat a b -> cat a c

-- instances for Maybe

instance Functor Maybe where
    fmap _ Nothing = Nothing
    fmap f (Just x) = Just $ f x

instance Monad Maybe where
    Just x >>= f = f x
    Nothing >>= _ = Nothing
    _ >> x = x
    return = Just

-- instances for List

instance Functor List where
    fmap = List.map

instance Monad List where
    xs >>= f = concatMap f xs
    _ >> xs = xs
    return x = Cons x List.Nil

-- instance for Tree

instance Functor Tree where
    fmap = Tree.map
