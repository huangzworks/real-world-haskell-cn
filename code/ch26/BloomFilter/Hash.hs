-- file: BloomFilter/Hash.hs
{-# LANGUAGE BangPatterns, ForeignFunctionInterface #-}
module BloomFilter.Hash
	(
	  Hashable(..)
	, hash
	, doubleHash
	) where
import Data.Bits ((.&.), shiftR)
import Foreign.Marshal.Array (withArrayLen)
import Control.Monad (foldM)
import Data.Word (Word32, Word64)
import Foreign.C.Types (CSize)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable, peek, sizeOf)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import System.IO.Unsafe (unsafePerformIO)
foreign import ccall unsafe "lookup3.h hashword2" hashWord2
	:: Ptr Word32 -> CSize -> Ptr Word32 -> Ptr Word32 -> IO ()
foreign import ccall unsafe "lookup3.h hashlittle2" hashLittle2
	:: Ptr a -> CSize -> Ptr Word32 -> Ptr Word32 -> IO ()

hashIO :: Ptr a    -- value to hash
	   -> CSize    -- number of bytes
	   -> Word64   -- salt
	   -> IO Word64
hashIO ptr bytes salt =
	with (fromIntegral salt) $ \sp -> do
	  let p1 = castPtr sp
		  p2 = castPtr sp `plusPtr` 4
	  go p1 p2
	  peek sp
  where go p1 p2
		  | bytes .&. 3 == 0 = hashWord2 (castPtr ptr) words p1 p2
		  | otherwise        = hashLittle2 ptr bytes p1 p2
		words = bytes `div` 4

class Hashable a where
	hashSalt :: Word64        -- ^ salt
			 -> a             -- ^ value to hash
			 -> Word64
hash :: Hashable a => a -> Word64
hash = hashSalt 0x106fc397cf62f64d3

hashStorable :: Storable a => Word64 -> a -> Word64
hashStorable salt k = unsafePerformIO . with k $ \ptr ->
					  hashIO ptr (fromIntegral (sizeOf k)) salt
instance Hashable Char   where hashSalt = hashStorable
instance Hashable Int    where hashSalt = hashStorable
instance Hashable Double where hashSalt = hashStorable

instance Storable a => Hashable a where
	hashSalt = hashStorable

hashList :: (Storable a) => Word64 -> [a] -> IO Word64
hashList salt xs =
	withArrayLen xs $ \len ptr ->
	  hashIO ptr (fromIntegral (len * sizeOf x)) salt
  where x = head xs
instance (Storable a) => Hashable [a] where
	hashSalt salt xs = unsafePerformIO $ hashList salt xs

hash2 :: (Hashable a) => a -> Word64 -> Word64
hash2 k salt = hashSalt salt k
instance (Hashable a, Hashable b) => Hashable (a,b) where
	hashSalt salt (a,b) = hash2 b . hash2 a $ salt
instance (Hashable a, Hashable b, Hashable c) => Hashable (a,b,c) where
	hashSalt salt (a,b,c) = hash2 c . hash2 b . hash2 a $ salt

hashByteString :: Word64 -> Strict.ByteString -> IO Word64
hashByteString salt bs = Strict.useAsCStringLen bs $ \(ptr, len) ->
						 hashIO ptr (fromIntegral len) salt
instance Hashable Strict.ByteString where
	hashSalt salt bs = unsafePerformIO $ hashByteString salt bs
rechunk :: Lazy.ByteString -> [Strict.ByteString]
rechunk s
	| Lazy.null s = []
	| otherwise   = let (pre,suf) = Lazy.splitAt chunkSize s
					in  repack pre : rechunk suf
	where repack    = Strict.concat . Lazy.toChunks
		  chunkSize = 64 * 1024
instance Hashable Lazy.ByteString where
	hashSalt salt bs = unsafePerformIO $
					   foldM hashByteString salt (rechunk bs)

doubleHash :: Hashable a => Int -> a -> [Word32]
doubleHash numHashes value = [h1 + h2 * i | i <- [0..num]]
	where h   = hashSalt 0x9150a946c4a8966e value
		  h1  = fromIntegral (h `shiftR` 32) .&. maxBound
		  h2  = fromIntegral h
		  num = fromIntegral numHashes