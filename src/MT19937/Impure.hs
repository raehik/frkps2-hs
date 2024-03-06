{- | MT19937 (32-bit) Mersenne Twister PRNG.

You may extract a single byte at a time.

Implemented with various unsafeties for efficiency.
All interaction is impure through IO.
One could write a pure interface. It'd be a pain though.
-}

{-# LANGUAGE RecordWildCards #-}

module MT19937.Impure where

import MT19937.Internal ( temper )

import Data.Bits
import Data.Word ( Word32 )

import Data.Array.IO ( IOUArray )
--import Data.Array.MArray
import Data.Array.Base ( unsafeNewArray_, unsafeWrite, unsafeRead )

import Data.IORef

-- | disgusting. do not peek inside this monstrosity
data MT19937 = MT19937
  { idx :: IORef Int
  , mt  :: IOUArray Int Word32
  }

init :: Word32 -> IO MT19937
init seed = do
    mt  <- initState seed
    idx <- newIORef 624
    pure MT19937{..}

extract :: MT19937 -> IO Word32
extract (MT19937 idx mt) = do
    idx' <- readIORef idx
    if idx' == 624
    then do
        twist mt
        mti <- unsafeRead mt 0
        writeIORef idx 1
        pure $ temper mti
    else do
        mti <- unsafeRead mt (fromIntegral idx')
        writeIORef idx (idx'+1)
        pure $ temper mti

twist :: IOUArray Int Word32 -> IO ()
twist mt' = go (0 :: Word32)
  where
    fI = fromIntegral
    m = 397
    a = 0x9908B0DF
    go = \case
      623 -> pure ()
      i   -> do
        mti  <- unsafeRead mt' (fI i)
        mti1 <- unsafeRead mt' (fI ((i+1) `mod` 623))
        mtim <- unsafeRead mt' (fI ((i+m) `mod` 623))
        let x    = (mti .&. 0x80000000) + (mti1 .&. 0x7FFFFFFF)
            mti' = mtim `xor` (x `shiftR` 1)
        if   x .&. 1 == 0
        then unsafeWrite mt' (fI i) mti'
        else unsafeWrite mt' (fI i) (mti' `xor` a)
        go (i+1)

-- TODO seed may need to be nonzero. (from original paper)
initState :: Word32 -> IO (IOUArray Int Word32)
initState seed = do
    mt' <- unsafeNewArray_ (0, 623)
    unsafeWrite mt' 0 seed
    go mt' (1 :: Word32)
  where
    f = 1812433253 -- 0x6c078965
    w = 32
    go mt' = \case
      623 -> pure mt'
      i   -> do
        prev <- unsafeRead mt' (fromIntegral (i-1))
        let mti = f * (prev `xor` (prev `shiftR` (w-2))) + i
        unsafeWrite mt' (fromIntegral i) mti
        go mt' (i+1)
