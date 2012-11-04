{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Categories where

import ITMOPrelude.Primitive
import ITMOPrelude.List
import ITMOPrelude.Tree

class Functor f where
    fmap :: (a -> b) -> f a -> f b

class Monad m where
    (>>=) :: m a -> (a -> m b) -> m b
    return :: a -> m a

class Category cat where
    id :: cat a a
    (.) :: cat b c -> cat a b -> cat a c

(>>) :: Monad m => m a -> m b -> m b
ma >> mb = ma >>= (\_ -> mb)

-- instance for (->)

instance Category (->) where
        id = \x -> x 
        (.) = (ITMOPrelude.Primitive..)

-- instances for Maybe

instance Functor Maybe where
    fmap _ Nothing = Nothing
    fmap f (Just x) = Just $ f x

instance Monad Maybe where
    Just x >>= f = f x
    Nothing >>= _ = Nothing
    return = Just

-- instances for List

instance Functor List where
    fmap = ITMOPrelude.List.map

instance Monad List where
    xs >>= f = concatMap f xs
    return x = Cons x ITMOPrelude.List.Nil

-- instance for Tree

instance Functor Tree where
    fmap = ITMOPrelude.Tree.map
