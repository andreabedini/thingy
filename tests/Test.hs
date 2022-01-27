{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.QuickCheck.Function
import Thingy

deriving via [a] instance Arbitrary a => Arbitrary (Thingy a)

deriving via [a] instance EqProp a => EqProp (Thingy a)

main = do
  quickBatch $ functor (undefined :: Thingy (Int, String, Char))
  quickBatch $ applicative (undefined :: Thingy (Int, String, Char))
