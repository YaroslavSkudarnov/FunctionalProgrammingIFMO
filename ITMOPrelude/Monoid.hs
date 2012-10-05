{-# LANGUAGE NoImplicitPrelude #-}
import ITMOPrelude.List --for test

class Monoid a where
    binOp :: a -> a -> a
    idElem :: a

--Test. Works for me.
instance Monoid (List a) where
    binOp = (++)
    idElem = Nil
