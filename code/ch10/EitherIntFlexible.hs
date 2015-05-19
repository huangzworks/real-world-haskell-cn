-- file: ch10/EitherIntFlexible.hs
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
-- -XOverlappingInstances is deprecated: instead use per-instance pragmas OVERLAPPING/OVERLAPPABLE/OVERLAPS

instance Functor (Either Int) where
    fmap _ (Left n)  = Left n
    fmap f (Right r) = Right (f r)