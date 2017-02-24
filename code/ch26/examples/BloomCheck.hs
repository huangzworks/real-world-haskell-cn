-- file: examples/BloomCheck.hs
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--module Main where
import BloomFilter.Hash (Hashable)
import Data.Word (Word8, Word32)
import System.Random (Random(..), RandomGen)
import Test.QuickCheck
import qualified BloomFilter.Easy as B
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy

handyCheck :: Testable a => Int -> a -> IO ()
handyCheck limit = check defaultConfig {
					 configMaxTest = limit
				   , configEvery   = \_ _ -> ""
				   }

falsePositive :: Gen Double
falsePositive = choose (epsilon, 1 - epsilon)
	where epsilon = 1e-6
(=~>) :: Either a b -> (b -> Bool) -> Bool
k =~> f = either (const True) f k
prop_one_present _ elt =
	forAll falsePositive $ \errRate ->
	  B.easyList errRate [elt] =~> \filt ->
		elt `B.elem` filt

	prop_all_present _ xs =
		forAll falsePositive $ \errRate ->
		  B.easyList errRate xs =~> \filt ->
			all (`B.elem` filt) xs

instance Arbitrary Lazy.ByteString where
	arbitrary = Lazy.pack `fmap` arbitrary
	coarbitrary = coarbitrary . Lazy.unpack
instance Arbitrary Strict.ByteString where
	arbitrary = Strict.pack `fmap` arbitrary
	coarbitrary = coarbitrary . Strict.unpack
instance Random Word8 where
  randomR = integralRandomR
  random = randomR (minBound, maxBound)
instance Arbitrary Word8 where
	arbitrary = choose (minBound, maxBound)
	coarbitrary = integralCoarbitrary

integralCoarbitrary n =
	variant $ if m >= 0 then 2*m else 2*(-m) + 1
  where m = fromIntegral n
integralRandomR (a,b) g = case randomR (c,d) g of
							(x,h) -> (fromIntegral x, h)
	where (c,d) = (fromIntegral a :: Integer,
				   fromIntegral b :: Integer)
instance Random Word32 where
  randomR = integralRandomR
  random = randomR (minBound, maxBound)
instance Arbitrary Word32 where
	arbitrary = choose (minBound, maxBound)
	coarbitrary = integralCoarbitrary

prop_suggest_try1 =
  forAll falsePositive $ \errRate ->
        forAll (choose (1,maxBound :: Word32)) $ \cap ->
          case B.suggestSizing (fromIntegral cap) errRate of
                Left err -> False
                Right (bits,hashes) -> bits > 0 && bits < maxBound && hashes > 0

prop_suggest_try2 =
        forAll falsePositive $ \errRate ->
          forAll (choose (1,fromIntegral maxWord32)) $ \cap ->
                let bestSize = fst . minimum $ B.sizings cap errRate
                in bestSize < fromIntegral maxWord32 ==>
                   either (const False) sane $ B.suggestSizing cap errRate
  where sane (bits,hashes) = bits > 0 && bits < maxBound && hashes > 0
                maxWord32 = maxBound :: Word32

prop_suggestions_sane =
        forAll falsePositive $ \errRate ->
          forAll (choose (1,fromIntegral maxWord32 `div` 8)) $ \cap ->
                let size = fst . minimum $ B.sizings cap errRate
                in size < fromIntegral maxWord32 ==>
                   either (const False) sane $ B.suggestSizing cap errRate
  where sane (bits,hashes) = bits > 0 && bits < maxBound && hashes > 0
                maxWord32 = maxBound :: Word32