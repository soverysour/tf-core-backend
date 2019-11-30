{-# LANGUAGE ScopedTypeVariables #-}

module App.Defs.Enums
  ( toSafeEnum
  ) where

toSafeEnum ::
     forall a. (Enum a, Bounded a)
  => Int
  -> Maybe a
toSafeEnum i =
  if fromEnum (maxBound :: a) < i || fromEnum (minBound :: a) > i
    then Nothing
    else Just $ toEnum i
