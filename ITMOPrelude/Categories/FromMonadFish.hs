{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module ITMOPrelude.Categories.FromMonadFish where
import ITMOPrelude.Categories.MonadFish

import ITMOPrelude.Primitive

-- ���
import ITMOPrelude.Categories
import ITMOPrelude.Categories.MonadJoin

-- ������ �� ���
instance MonadFish m => Monad m where
    a >>= f = ((\x -> x) >=> f) a
    _ >> y = y
    return = returnFish

instance MonadFish m => Functor m where
    fmap f a = ((\x -> x) >=> (\y -> returnFish (f y))) a

instance MonadFish m => MonadJoin m where
    returnJoin = returnFish
    join = ((\x -> x) >=> (\y -> y))
