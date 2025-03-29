module FrkPs2.FRK2.Packer where

import Foreign.Ptr ( Ptr )
import Data.Word ( Word8 )
import System.Directory ( createDirectoryIfMissing )
import System.FilePath ( takeDirectory )
import System.IO

-- Start with an implementation that just prints filepath and size.
-- Then write one that writes files to a configured folder.
-- Could also write one that writes directly to another archive format!
-- gonna need a type class for converting a btw

{-

-- | fp is filepath. 'FilePath' (filesystem) or 'ByteString' (virtual etc.).
data Packer fp :: Effect where
    -- | Relative, unique filepaths only.
    PackFile :: fp -> Ptr Word8 -> Int -> Packer fp m ()
makeEffect ''Packer

runPackerFolder
    :: IOE :> es
    => FilePath -> Handle -> Eff (Packer FilePath : es) a -> Eff es a
runPackerFolder fpRoot hdlSrc = interpret $ \_ -> \case
  PackFile fp os len -> liftIO $ do
    let fpAbs    = fpRoot<>"/"<>fp
        fpAbsDir = takeDirectory fpAbs
    createDirectoryIfMissing True fpAbsDir
    withBinaryFile fpAbs WriteMode $ \hdlDest ->
        hSeek hdl AbsoluteSeek os
        hPutBuf hdl _ len

-}

-- | Copy bytes from one handle to another.
--
-- TODO I feel like there should be a faster special implementation here?
-- But maybe not. Shame.
--
-- Actually, wow this sucks. I have to allocaBytes _every single time_?
-- I should just calculate the largest buffer I'll need and reuse that.
--
-- OH LMAO I SHOULD DO THIS? OK BUD
-- https://github.com/coreutils/coreutils/blob/be3a216ab447d2f82daa3ff5851111856467e093/src/ioblksize.h
-- adapted from https://hackage.haskell.org/package/directory-1.3.8.3/docs/src/System.Directory.Internal.Common.html#copyHandleData
-- with an updated value. I should allocate a 256KiB buffer _before_ getting
-- here, and reuse it everywhere. LMAO
--hCopy :: Handle -> Handle -> Int -> IO ()
