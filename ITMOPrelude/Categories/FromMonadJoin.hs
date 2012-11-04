{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module ITMOPrelude.Categories.FromMonadJoin where
import ITMOPrelude.Categories.MonadJoin

import ITMOPrelude.Primitive

-- Эти
import ITMOPrelude.Categories
import ITMOPrelude.Categories.MonadFish

-- делаем из нас

instance MonadJoin m => Monad m where
    x >>= f = join $ fmap f x
    _ >> y = y
    return = returnJoin    

instance MonadJoin m => MonadFish m where
    returnFish = returnJoin
    (f >=> g) x = join $ fmap g $ f x
