{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Categories where

import ITMOPrelude.List
import ITMOPrelude.Primitive

class Functor f where  
    fmap :: (a -> b) -> f a -> f b

class Monad m where  
    (>>=) :: m a -> (a -> m b) -> m b
    (>>) :: m a -> m b -> m b
    return :: a -> m a

class Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

class Arrow a where
    arr :: (b -> c) -> a b c
    (>>>) :: a b c -> a c d -> a b d
    first :: a b c -> a (Pair b d) (pair c d)

--Tests

instance Functor List where
    fmap = map

instance Monad List where
    xs >>= f = concatMap f xs
    _ >> ys = ys
    return x = Cons x Nil

instance Applicative List where
    pure = return
    fs <*> xs = fs >>= (\f -> (xs >>= (\x -> pure (f x))))
