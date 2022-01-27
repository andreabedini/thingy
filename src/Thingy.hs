{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Thingy where

import Control.Applicative

newtype Thingy a = Thingy { unThingy :: [a] }
  deriving (Eq, Show, Functor)

instance Applicative Thingy where

  pure :: a -> Thingy a
  pure x = Thingy [x]

  (<*>) :: Thingy (a -> b) -> Thingy a -> Thingy b
  (Thingy _ ) <*> (Thingy []) = Thingy []
  (Thingy []) <*> (Thingy _ ) = Thingy []
  (Thingy (f:fs)) <*> (Thingy (x:xs)) = Thingy $ f x : [ f' x | f' <- fs ] ++ [ f x | x <- xs ]

