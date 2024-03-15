{- | The "FRK2" encoded archive format that Fuuraiki (2006) uses.

Data is encoded using a constant-seed MT19937 (Mersenne Twister).
Conveniently, this means the same operation both encodes and decodes. Indeed:

    code (code x) === x

-}

module FrkPs2.FRK2 where

import GHC.ForeignPtr ( unsafeWithForeignPtr )
import Foreign.Marshal.Utils ( copyBytes )

import Data.ByteString.Internal qualified as B
import Data.ByteString          qualified as B

import System.IO ( Handle, hGetBuf, hPutBuf )
import Foreign.Marshal.Alloc ( allocaBytes )

import FrkPs2.FRK2.Internal ( codeBuf, initialPrng )

-- can we use unsafeWithForeignPtr ? idk probably
code :: B.ByteString -> B.ByteString
code (B.BS fptr len) =
    B.unsafeCreate len $ \dest -> unsafeWithForeignPtr fptr $ \src -> do
        copyBytes dest src len
        _prng' <- codeBuf dest len initialPrng
        pure ()

codeFile :: FilePath -> IO B.ByteString
codeFile fp = code <$> B.readFile fp

-- | Code from one handle to another, using the given buffer size.
--
-- I don't know what a good buffer size is for this stuff. So _you_ decide! >:)
codeHandle :: Int -> Handle -> Handle -> IO ()
codeHandle bufLen hr hw = allocaBytes bufLen (go initialPrng)
  where
    go prng buf = do
        bufLenWritten <- hGetBuf hr buf bufLen
        if   bufLenWritten < bufLen
        then do _prng' <- codeBuf buf bufLenWritten prng
                hPutBuf hw buf bufLenWritten
                pure ()
        else do prng' <- codeBuf buf bufLenWritten prng
                -- safely assume bufLen == bufLenWritten so either is OK
                hPutBuf hw buf bufLenWritten
                go prng' buf
