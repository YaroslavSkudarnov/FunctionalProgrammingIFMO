{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module ITMOPrelude.Categories.FromMonad where
import ITMOPrelude.Categories hiding ((.))

import ITMOPrelude.Primitive

-- Эти
import ITMOPrelude.Categories.MonadJoin
import ITMOPrelude.Categories.MonadFish

-- делаем нас

instance Monad m => MonadFish m where
    returnFish = return
    (f >=> g) a = f a >>= g

instance Monad m => Functor m where
    fmap f a = a >>= (return . f)

instance Monad m => MonadJoin m where
    returnJoin = return
    join a = a >>= (\x -> x)
