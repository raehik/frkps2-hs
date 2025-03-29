{-# LANGUAGE UndecidableInstances #-} -- for generic cblen

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

import Strongweak
import Strongweak.Generic
import Binrep
import Binrep.Instances.Strongweak.Type.Magic
import Binrep.Instances.Strongweak.Util.ByteOrder
import Data.Text.Builder.Linear qualified as TBL
import Binrep.Type.Derived.NullTermPadded
import Binrep.Type.NullTerminated ( type NullTerminate )
import Rerefined ( type Refine )
import Binrep.Type.Magic
import Binrep.Util.ByteOrder
import Binrep.Common.Via.Generically.NonSum
import Data.Word
import GHC.Generics ( Generic, Generically(..) )
import Data.Typeable ( Typeable )
import MT19937.Pure ( MT19937 )
import Foreign.Ptr ( Ptr, plusPtr )

import Data.Bits ( (.^.) )
import System.Exit ( die )

-- | Coding status.
data Encoded = Cipher | Plain

-- | Helper for bidirectional coding operations.
type family Code (enc :: Encoded) :: Encoded where
    Code 'Cipher = 'Plain
    Code 'Plain  = 'Cipher

type W32LE = ByteOrdered 'LittleEndian Word32

-- | prepared file table (b for "binary")
type FileTableEntryB = FileTableEntry 'Strong B.ByteString

data FileTableEntry (s :: Strength) a = FileTableEntry
  { buf    :: SW s (Magic '[0x00, 0x00, 0x00, 0x00])
  -- ^ Used in runtime, maybe always zeroed before.
  , unk1_2 :: SW s (Magic '[0x00, 0x00])
  -- ^ unknown short (2 bytes), I only see zeroes though so w/e lol
  , flags  :: Word8
  -- ^ apparently based on some code reading bits from it
  , file   :: SW s (Magic '[0x00])
  -- ^ index in game.cfg of owning file
  , filesize :: SW s W32LE
  , offset   :: SW s W32LE
  -- ^ byte offset in file. should be multiple of 0x800!
  , name     :: SW s (NullTermPadded 16 a)
  } deriving stock Generic
deriving instance Show a => Show (FileTableEntry 'Weak   a)
deriving instance Show a => Show (FileTableEntry 'Strong a)

-- v These instances are so fucking cool.
deriving via GenericallyNonSum (FileTableEntry 'Strong a)
    instance IsCBLen (FileTableEntry 'Strong a)
deriving via ViaCBLen (FileTableEntry 'Strong a)
    instance BLen    (FileTableEntry 'Strong a)
deriving via Generically (FileTableEntry 'Strong a)
    instance (BLen a, Put a) => PutC (FileTableEntry 'Strong a)
deriving via Generically (FileTableEntry 'Strong a)
    instance Get a => GetC (FileTableEntry 'Strong a)

instance Weaken   (FileTableEntry 'Strong a) where
    type Weakened (FileTableEntry 'Strong a) = FileTableEntry 'Weak a
    weaken = weakenGeneric
instance (BLen a, Typeable a, Refine NullTerminate a)
  => Strengthen (FileTableEntry 'Strong a) where
    strengthen = strengthenGeneric

-- buf is immutable and @entries@ * @'cblen' \@'FileTableEntryB'@ bytes long
-- and needs to have been decoded already
-- this type looks REALLY weird. but it's maybe safe? because it comes with a
-- bunch of expectations??
-- kinda want to add IO back in but the get runs without it :| weird situation
getFileTable
    :: Ptr Word8 -> Int -> Either (ParseError Int TBL.Builder) [FileTableEntryB]
getFileTable src entryCount = go [] src
  where
    entryLen = cblen @FileTableEntryB
    go entries buf
      | buf == bufEnd = Right entries
      | otherwise = do
            case unsafeRunGetCPtr buf of
              Right entry -> go (entry : entries) (buf `plusPtr` entryLen)
              Left  e     -> Left e
    bufEnd = src `plusPtr` (entryCount * entryLen)

-- can we use unsafeWithForeignPtr ? idk probably
codeBS :: B.ByteString -> B.ByteString
codeBS (B.BS fptr len) =
    B.unsafeCreate len $ \dest -> unsafeWithForeignPtr fptr $ \src -> do
        copyBytes dest src len
        _prng' <- codeBuf dest len initialPrng
        pure ()

-- can we use unsafeWithForeignPtr ? idk probably
-- bit ugly since we have to use UptoN to return something else :(
codeBS' :: B.ByteString -> MT19937 -> (B.ByteString, MT19937)
codeBS' (B.BS fptr len) prng0 =
    B.unsafeCreateUptoN' len $ \dest -> unsafeWithForeignPtr fptr $ \src -> do
        copyBytes dest src len
        prng1 <- codeBuf dest len prng0
        pure (len, prng1)

codeFile :: FilePath -> IO B.ByteString
codeFile fp = codeBS <$> B.readFile fp

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

data Frk2Header (s :: Strength) = Frk2Header
  { magicFrk2   :: SW s (Magic "FRK2")
  , ftblEntries :: SW s W32LE
  , unk1        :: SW s W32LE
  , unk2        :: SW s W32LE
  , unk3        :: SW s W32LE
  , unk4        :: SW s W32LE
  , unk5        :: SW s W32LE
  , unk6        :: SW s W32LE
  } deriving stock Generic
deriving stock instance Show (Frk2Header 'Strong)

deriving via GenericallyNonSum (Frk2Header 'Strong)
    instance IsCBLen (Frk2Header 'Strong)
deriving via ViaCBLen    (Frk2Header 'Strong) instance BLen (Frk2Header 'Strong)
deriving via Generically (Frk2Header 'Strong) instance PutC (Frk2Header 'Strong)
deriving via Generically (Frk2Header 'Strong) instance GetC (Frk2Header 'Strong)

instance Weaken   (Frk2Header 'Strong) where
    type Weakened (Frk2Header 'Strong) = Frk2Header 'Weak
    weaken = weakenGeneric
instance Strengthen (Frk2Header 'Strong) where
    strengthen = strengthenGeneric

-- | see 0x0025bd94 in exec
codeVtblEntries :: Word32 -> Word32
codeVtblEntries = (.^.) 0xF76C0531

-- | use with 'withBinaryFile'
--
-- parses filetable all-in-one
--
-- TODO add sanity checks (primarily, assert that filetable fits in file before
-- allocating that buffer)
getFrk2EntriesHandle
    :: Handle
    -> IO (Either (ParseError Int TBL.Builder) [FileTableEntryB])
getFrk2EntriesHandle hdl = allocaBytes 0x20 $ \buf -> do
    -- TODO lol I could even reuse the 256KiB buf here if I want... probably bad
    -- though ^^;
    hGetBuf' hdl buf 0x20
    case unsafeRunGetCPtr @(Frk2Header 'Strong) buf of
      Left  e          -> pure $ Left e
      Right frk2Header -> do
        let ftblEntries' =
                fromIntegral $ codeVtblEntries $ unByteOrdered $ ftblEntries frk2Header
            ftblLen      = cblen @FileTableEntryB * ftblEntries'
        -- | TODO starting from here, we should just reuse our 256KiB IO buf
        --   instead of re-allocating
        allocaBytes ftblLen $ \buf' -> do
            hGetBuf' hdl buf' ftblLen
            _prng' <- codeBuf buf' ftblLen initialPrng
            pure $ getFileTable buf' ftblEntries'

-- | Get all bytes we ask for, or die. TODO bad but easy!
hGetBuf' :: Handle -> Ptr a -> Int -> IO ()
hGetBuf' hdl buf len = do
    len' <- hGetBuf hdl buf len
    if len == len' then pure () else die "small file"

{-
asdf
    :: (Packer :> es)
    => Handle -> FilePath -> [FileTableEntryB] -> Eff es a
asdf hdl fpRoot es = do
    createDirectoryIfMissing True fpRoot
    traverse_ (\e -> packFile filesize e packFile packFile
-}
