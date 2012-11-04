{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module ITMOPrelude.Categories.FromMonadFish where
import ITMOPrelude.Categories.MonadFish

import ITMOPrelude.Primitive

-- Эти
import ITMOPrelude.Categories hiding ((.))
import ITMOPrelude.Categories.MonadJoin

-- делаем из нас
instance MonadFish m => Monad m where
    a >>= f = ((\x -> x) >=> f) a
    _ >> y = y
    return = returnFish

instance MonadFish m => Functor m where
    fmap f a = ((\x -> x) >=> (returnFish . f)) a

instance MonadFish m => MonadJoin m where
    returnJoin = returnFish
    join = ((\x -> x) >=> (\y -> y))
