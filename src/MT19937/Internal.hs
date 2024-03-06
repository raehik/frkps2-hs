-- | Shared MT19937 internals functions.

module MT19937.Internal where

import Data.Bits

-- | MT19937 tempering function.
temper :: (Num a, Bits a) => a -> a
temper x = z
  where
    y1 = x  `xor` ((x  `shiftR` u) .&. d)
    y2 = y1 `xor` ((y1 `shiftL` s) .&. b)
    y3 = y2 `xor` ((y2 `shiftL` t) .&. c)
    z  = y3 `xor`  (y3 `shiftR` l)
    u = 11
    d = 0xFFFFFFFF
    s = 7
    b = 0x9D2C5680
    t = 15
    c = 0xEFC60000
    l = 18

