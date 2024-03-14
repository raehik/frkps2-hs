{- | The "FRK2" encoded archive format that Fuuraiki (2006) uses.

Data is encoded using a constant-seed MT19937 (Mersenne Twister).
Conveniently, this means the same operation both encodes and decodes. Indeed:

    code (code x) === x

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

initialPrng :: MT19937
initialPrng = MT19937.skip 1 $ MT19937.init 0xFACEFACE

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
        _prng' <- code' dest len initialPrng
        pure ()

codeFile :: FilePath -> IO B.ByteString
codeFile fp = code <$> B.readFile fp

codeHandle :: Int -> Handle -> Handle -> IO ()
codeHandle bufLen hr hw = allocaBytes bufLen (go initialPrng)
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
