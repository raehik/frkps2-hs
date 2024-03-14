{-
Coding is really, really slow and I don't know why.
Coding cmn.bin (2.9MB) takes ~15s.
Surely this is wrong.
Disabling MT19937 makes it ~2s. Still slow.
-}

module FrkPs2.FRK2 where

import Raehik.MT19937.Pure qualified as MT19937
import Raehik.MT19937.Pure ( MT19937 )
import Data.Bits ( xor, complement )
import Data.Word

import Control.Monad.Primitive ( PrimMonad )
import Data.Primitive.Ptr ( readOffPtr, writeOffPtr )
import Foreign.Ptr ( Ptr, castPtr )
import GHC.ForeignPtr ( unsafeWithForeignPtr )
import Foreign.Marshal.Utils ( copyBytes )

import Data.ByteString.Internal qualified as B
import Data.ByteString          qualified as B

import System.IO ( Handle, hGetBuf, hPutBuf )
import Foreign.Marshal.Alloc ( allocaBytes )

-- | Encodes or decodes.
--
-- @code rng w (fst (code rng w)) === w@
codeW32 :: MT19937 -> Word32 -> (Word32, MT19937)
codeW32 prng0 w = (w `xor` cipher, prng2)
  where
    cipher = complement (rnd1 * rnd2)
    (rnd1, prng1) = MT19937.extract prng0
    (rnd2, prng2) = MT19937.extract prng1

-- can we use unsafeWithForeignPtr ? idk probably
code :: B.ByteString -> B.ByteString
code (B.BS fptr len) =
    B.unsafeCreate len $ \dest -> unsafeWithForeignPtr fptr $ \src -> do
        copyBytes dest src len
        _prng' <- code' dest len prng
        pure ()
  where
    prng = MT19937.skip 1 $ MT19937.init 0xFACEFACE

codeFile :: FilePath -> IO B.ByteString
codeFile fp = code <$> B.readFile fp

codeHandle :: Int -> Handle -> Handle -> IO ()
codeHandle bufLen hr hw = allocaBytes bufLen (go prng0)
  where
    go :: MT19937 -> Ptr Word8 -> IO ()
    go prng buf = do
        bufLenWritten <- hGetBuf hr buf bufLen
        if   bufLenWritten < bufLen
        then do _prng' <- code' buf bufLenWritten prng
                hPutBuf hw buf bufLenWritten
                pure ()
        else do prng' <- code' buf bufLenWritten prng
                -- safely assume bufLen == bufLenWritten so either is OK
                hPutBuf hw buf bufLenWritten
                go prng' buf
    prng0 = MT19937.skip 1 $ MT19937.init 0xFACEFACE

-- buf is mutable data
-- taking Ptr because looks like copy prims work on them (see GHC.IO.Handle)
-- maybe have to worry about the casts if alignment matters :(
-- but it's surely fine it wouldn't make sense!
code'
    :: PrimMonad m
    => Ptr Word8 -> Int -> MT19937 -> m MT19937
code' buf len prng0 = goW32 prng0 0
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
    goW8s prng _i = pure prng -- TODO idk how to handle these exactly

{-

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

-}

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
