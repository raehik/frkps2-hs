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
import Binrep.Type.NullPadded
import Binrep.Type.NullTerminated
import Binrep.Type.Magic
import Binrep.Util.ByteOrder
import Binrep.CBLen
import Binrep.CBLen.Generic
import Data.Word
import GHC.Generics ( Generic )
import Data.Typeable ( Typeable )
import Binrep
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

data FileTable (s :: Strength) a = FileTable
  { entries :: [SW s (FileTableEntry s a)]
  } deriving stock Generic
deriving stock instance Show a => Show (FileTable 'Strong a)

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
  , name     :: SW s (NullPadded 16 (NullTerminated a))
  } deriving stock Generic
deriving instance Show a => Show (FileTableEntry 'Weak   a)
deriving instance Show a => Show (FileTableEntry 'Strong a)

-- v These instances are so fucking cool.
instance IsCBLen (FileTableEntry 'Strong a) where
    type CBLen (FileTableEntry 'Strong a) =
        CBLenGenericNonSum (FileTableEntry 'Strong a)
deriving via (ViaCBLen (FileTableEntry 'Strong a)) instance
    BLen (FileTableEntry 'Strong a)

instance Weaken (FileTableEntry 'Strong a) where
    type Weak   (FileTableEntry 'Strong a) = FileTableEntry 'Weak a
    weaken = weakenGeneric
instance (BLen a, Typeable a) => Strengthen (FileTableEntry 'Strong a) where
    strengthen = strengthenGeneric

-- Look at this instance. @Get a@ constraint? Fucking incredible.
instance Get a => GetC (FileTableEntry 'Strong a) where
    getC = getGenericStruct

-- buf is immutable and @entries@ * @'cblen' \@'FileTableEntryB'@ bytes long
-- and needs to have been decoded already
-- this type looks REALLY weird. but it's maybe safe? because it comes with a
-- bunch of expectations??
getFileTable :: Ptr Word8 -> Int -> Either E (FileTable 'Strong B.ByteString)
getFileTable src entryCount = go [] src
  where
    entryLen = cblen @FileTableEntryB
    go entries buf
      | buf == bufEnd = Right $ FileTable entries
      | otherwise = do
            case unsafeRunGetCPtr buf of
              Left  e     -> Left e
              Right entry -> go (entry : entries) (buf `plusPtr` entryLen)
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

-- | see 0x0025bd94 in exec
codeVtblEntries :: Word32 -> Word32
codeVtblEntries = (.^.) 0xF76C0531

-- fresh handle plz (pointing to FRK2)
getFrk2EntriesHandle
    :: Handle
    -> IO (Either E (FileTable 'Strong B.ByteString))
getFrk2EntriesHandle hdl = allocaBytes 0x20 $ \buf -> do
    hGetBuf' hdl buf 0x20
    case unsafeRunGetCPtr @(Frk2Header 'Strong) buf of
      Left  e          -> pure $ Left e
      Right frk2Header -> do
        let ftblEntries' =
                fromIntegral $ codeVtblEntries $ unByteOrdered $ ftblEntries frk2Header
            ftblLen      = cblen @FileTableEntryB * ftblEntries'
        allocaBytes ftblLen $ \buf' -> do
            hGetBuf' hdl buf' ftblLen
            _prng' <- codeBuf buf' ftblLen initialPrng
            pure $ getFileTable buf' ftblEntries'

-- | Get all bytes we ask for, or die. TODO bad but easy!
hGetBuf' :: Handle -> Ptr a -> Int -> IO ()
hGetBuf' hdl buf len = do
    len' <- hGetBuf hdl buf len
    if len == len' then pure () else die "small file"

instance IsCBLen (Frk2Header 'Strong) where
    type CBLen (Frk2Header 'Strong) =
        CBLenGenericNonSum (Frk2Header 'Strong)
deriving via (ViaCBLen (Frk2Header 'Strong)) instance
    BLen (Frk2Header 'Strong)

instance Weaken (Frk2Header 'Strong) where
    type Weak   (Frk2Header 'Strong) = Frk2Header 'Weak
    weaken = weakenGeneric
instance Strengthen (Frk2Header 'Strong) where
    strengthen = strengthenGeneric

instance GetC (Frk2Header 'Strong) where
    getC = getGenericStruct
