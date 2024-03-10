module FrkPs2.FRK2 where

import Raehik.MT19937.Pure qualified as MT19937
import Raehik.MT19937.Pure ( MT19937 )
import Data.Bits ( xor, complement )
import Data.Word ( Word32 )
import Data.ByteString ( ByteString )

rngSeed = 0xFACEFACE

-- | Encodes or decodes.
--
-- @code rng w (fst (code rng w)) === w@
codeW32 :: MT19937 -> Word32 -> (Word32, MT19937)
codeW32 rng w = (w `xor` cipher, rng'')
  where
    cipher = complement (rnd1 `xor` rnd2)
    (rnd1, rng')  = MT19937.extract rng
    (rnd2, rng'') = MT19937.extract rng'

codeBlock :: MT19937 -> ByteString -> (ByteString, MT19937)
code :: ByteString -> ByteString

type instance DispatchOf Coder = Dynamic
data Coder :: Effect where
    Finished    :: Coder m Bool
    NextBlock   :: Coder m ByteString
    NextMT19937 :: Coder m Word32
    WriteBlock  :: ByteString -> Coder m ()

nextMT19937 :: (HasCallStack, Coder :> es) => Eff es Word32
nextMT19937 = send NextMT19937

codeW32 :: Coder :> es => Word32 -> Eff es Word32
codeW32 w32 = do
    rng1 <- nextMT19937
    rng2 <- nextMT19937
    let cipher = complement (rng1 * rng2)
    pure (cipher ^ w32)

codeBlock :: Coder :> es => ByteString -> Eff es ()
codeBlock block = do
    let (w32Bs, block') = B.splitAt 4 block

{-

type instance DispatchOf Coder = Dynamic
data Coder :: Effect where
    NextW32     :: Coder m Word32
    NextMT19937 :: Coder m Word32
    WriteW32    :: Word32 -> Coder m ()

nextW32 :: (HasCallStack, Coder :> es) => Eff es Word32
nextW32 = send Next32

writeW32 :: (HasCallStack, Coder :> es) => Word32 -> Eff es ()
writeW32 w32 = send (Write32 w32)

codeW32 :: Coder :> es => Eff es ()
codeW32 = do
    w32 <- nextW32
    rng1 <- nextMT19937
    rng2 <- nextMT19937
    let cipher = complement(rng1 * rng2)
    writeW32 (cipher ^ w32)

code :: Eff es ()
code = ifM more (codeW32 *> code) (pure ())

-- | Like @if@, but where the test can be monadic. (from extra)
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b t f = do b <- b; if b then t else f

-}
