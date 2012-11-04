{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.IO where

import ITMOPrelude.Primitive
import ITMOPrelude.List
import ITMOPrelude.Categories

data RealWorld = RealWorld
    { stdIn :: List Nat
    , stdOut :: List Nat
    , exitCode :: Nat }

type IO a = State RealWorld a

getNat :: IO Nat
getNat = State $ \s -> (RealWorld { stdIn = tail $ stdIn s, stdOut = stdOut s, exitCode = exitCode s}, head $ stdIn s)

putNat :: Nat -> IO ()
putNat x = State $ \s -> (RealWorld { stdIn = stdIn s, stdOut = Cons x $ stdOut s, exitCode = exitCode s},())

setExitCode :: Nat -> IO ()
setExitCode x = State $ \s -> (RealWorld {stdIn = stdIn s, stdOut = stdOut s, exitCode = x}, ())
