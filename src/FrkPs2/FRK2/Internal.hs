-- | General FRK2 bits.

module FrkPs2.FRK2.Internal where

import MT19937.Pure qualified as MT19937
import MT19937.Pure ( MT19937 )
import Data.Bits ( xor, complement )
import Data.Word

import Control.Monad.Primitive ( PrimMonad )
import Data.Primitive.Ptr ( readOffPtr, writeOffPtr )
import Foreign.Ptr ( Ptr, castPtr )

initialPrng :: MT19937
initialPrng = MT19937.skip 1 $ MT19937.init 0xFACEFACE

{- | Code the data in a mutable buffer given its length and the current MT19937
     PRNG state. Returns the next PRNG state, and has changed the buffer.

Codes in 32-bit words. Ignores any final bytes if buffer isn't a multiple of 4
(because I don't know how to handle them).

This can be used to implement efficient coding for any interface (bytestrings,
handles, probably sockets too).
-}
codeBuf
    :: PrimMonad m
    => Ptr Word8 -> Int -> MT19937 -> m MT19937
codeBuf buf len prng0 = goW32 prng0 0
  where
    bufW32 :: Ptr Word32 = castPtr buf
    (w32s, _w8s) = len `quotRem` 4
    goW32 prng i
      | i == w32s = goW8s prng (0 :: Int)
      | otherwise = do
            w32 <- readOffPtr bufW32 i
            let (w32Coded, prng') = codeW32 prng w32
            writeOffPtr bufW32 i w32Coded
            goW32 prng' (i+1)
    goW8s prng _i = pure prng

-- | Code the given word.
--
-- @code rng w (fst (code rng w)) === w@
codeW32 :: MT19937 -> Word32 -> (Word32, MT19937)
codeW32 prng0 w = (w `xor` cipher, prng2)
  where
    cipher = complement (rnd1 * rnd2)
    (rnd1, prng1) = MT19937.extract prng0
    (rnd2, prng2) = MT19937.extract prng1
