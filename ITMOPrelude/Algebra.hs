{-# LANGUAGE NoImplicitPrelude, FlexibleInstances #-}

module ITMOPrelude.Algebra where

import ITMOPrelude.List
import ITMOPrelude.Primitive
import Prelude (Show,Read)

class Monoid a where
    binOp :: a -> a -> a
    idElem :: a

class Monoid a => Group a where
    revElem :: a -> a

-- instances for Unit

instance Monoid Unit where
    binOp _ _ = idElem
    idElem = Unit

instance Group Unit where
    revElem _ = Unit

-- instance for Maybe
instance Monoid (Maybe a) where
    binOp Nothing x = x
    binOp x _ = x

    idElem = Nothing

-- instances for Bool
newtype AllBool = AllBool {getAllBool :: Bool} deriving (Show,Read)

instance Monoid AllBool where
    binOp x y = AllBool $ (getAllBool x) && (getAllBool y)

    idElem = AllBool True

newtype AnyBool = AnyBool {getAnyBool :: Bool} deriving (Show,Read)

instance Monoid AnyBool where
    binOp x y = AnyBool $ (getAnyBool x) || (getAnyBool y)

    idElem = AnyBool False

-- instances for Tri
instance Monoid Tri where
    binOp EQ x = x
    binOp x _ = x

    idElem = EQ

-- instances for numbers
newtype Sum a = Sum {getSum :: a} deriving (Show,Read)

instance Monoid (Sum Nat) where
    binOp x y = Sum $ (getSum x) +. (getSum y)
    
    idElem = Sum Zero

instance Monoid (Sum Int) where
    binOp x y = Sum $ (getSum x) .+. (getSum y)
    
    idElem = Sum intZero

instance Group (Sum Int) where
    revElem x = Sum $ intNeg $ getSum x

instance Monoid (Sum Rat) where
    binOp x y = Sum $ (getSum x) %+ (getSum y)
    
    idElem = Sum $ Rat intZero natOne

instance Group (Sum Rat) where
    revElem x = Sum $ ratNeg $ getSum x

newtype Product a = Product {getProduct :: a} deriving (Show,Read)

instance Monoid (Product Nat) where
    binOp x y = Product $ (getProduct x) *. (getProduct y)
    
    idElem = Product natOne

instance Monoid (Product Int) where
    binOp x y = Product $ (getProduct x) .*. (getProduct y)
    
    idElem = Product intOne

instance Monoid (Product Rat) where
    binOp x y = Product $ (getProduct x) %* (getProduct y)
    
    idElem = Product $ Rat intOne natOne

instance Group (Product Rat) where
    revElem x = Product $ ratInv $ getProduct x

-- instance for List
instance Monoid (List a) where
    binOp = (++)

    idElem = Nil
