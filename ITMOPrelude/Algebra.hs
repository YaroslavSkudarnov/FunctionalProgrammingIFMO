{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Algebra where

import ITMOPrelude.List

class Group a where
    binOpGr :: a -> a -> a
    idElemGr :: a
    revElemGr :: a

class Monoid a where
    binOpMon :: a -> a -> a
    idElemMon :: a

--Test
instance Monoid (List a) where
    binOpMon = (++)
    idElemMon = Nil
