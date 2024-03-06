{- | Efficient, pure, standard 32-bit MT19937 Mersenne Twister implementation.

Note that seeding with 0 may result in poor randomness.
-}

module Raehik.MT19937.Pure
  ( MT19937(idx, mt), Raehik.MT19937.Pure.init, init', extract
  ) where

import Raehik.MT19937.Internal ( temper, twist )

import Data.Word ( Word32 )
import Data.Bits

import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM

-- | MT19937 state, holding the state vector and the current index.
data MT19937 = MT19937
  { idx :: Int
  , mt  :: VU.Vector Word32
  } deriving stock Show

-- | Initialize an MT19937 with the given seed.
init :: Word32 -> MT19937
init seed = MT19937 { idx = 0, mt = initState }
  where
    initState = VU.create $ do
        mt' <- VUM.unsafeNew 623
        VUM.unsafeWrite mt' 0 seed
        initHelper mt' 1
        twist mt'
        pure mt'

-- | Initialize an MT19937 with the given seed, without pre-twisting.
init' :: Word32 -> MT19937
init' seed = MT19937 { idx = 624, mt = initState }
  where
    initState = VU.create $ do
        mt' <- VUM.unsafeNew 623
        VUM.unsafeWrite mt' 0 seed
        initHelper mt' 1
        pure mt'

-- separated out due to ST scoping issues idk
initHelper
    :: VUM.PrimMonad m => VU.MVector (VUM.PrimState m) Word32 -> Word32 -> m ()
initHelper mt' = \case
  623 -> pure ()
  i   -> do
    prev <- VUM.unsafeRead mt' (fromIntegral (i-1))
    let mti = f * (prev `xor` (prev `shiftR` (w-2))) + i
    VUM.unsafeWrite mt' (fromIntegral i) mti
    initHelper mt' (i+1)
  where
    f = 1812433253 -- 0x6c078965
    w = 32

-- | Extract the next random byte and return the updated state.
extract :: MT19937 -> (Word32, MT19937)
extract (MT19937 idx mt) = do
    if idx == 624 then
        let mt' = VU.modify twist mt
            w   = temper (mt VU.! 0)
        in  (w, MT19937 1       mt')
    else
        let w   = temper (mt VU.! idx)
        in  (w, MT19937 (idx+1) mt)