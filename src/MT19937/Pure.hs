module MT19937.Pure ( MT19937, MT19937.Pure.init, extract ) where

import MT19937.Internal ( temper )
import MT19937.Impure qualified as Impure

import Data.Array.Unboxed ( UArray )
import Data.Array.Base ( unsafeAt, freeze, thaw )
import System.IO.Unsafe ( unsafePerformIO )

import Data.Word ( Word32 )

data MT19937 = MT19937
  { idx :: Int
  , mt  :: UArray Int Word32
  } deriving stock Show

init :: Word32 -> MT19937
init = error "TODO"

extract :: MT19937 -> (Word32, MT19937)
extract (MT19937 idx mt) = do
    if idx == 624 then
        let mt' = twist mt
            w   = temper (unsafeAt mt 0)
        in  (w, MT19937 1       mt')
    else
        let w   = temper (unsafeAt mt idx)
        in  (w, MT19937 (idx+1) mt)

-- not sure how twist purely, so we copy to mutable, then copy back :(
-- we can twist purely if we pause in the middle at 397. this also saves on the
-- mods
twist :: UArray Int Word32 -> UArray Int Word32
twist mt = unsafePerformIO $ do
    mtMut <- thaw mt
    Impure.twist mtMut
    mt' <- freeze mtMut
    pure mt'

twist' :: UArray Int Word32 -> UArray Int Word32
twist' mt = 
    mtMut <- thaw mt
    Impure.twist mtMut
    mt' <- freeze mtMut
    pure mt'
