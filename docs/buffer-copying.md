https://hackage.haskell.org/package/ghc-9.8.2/docs/src/GHC.SysTools.html#copyHandle

Internal function. Uses 8k buffer size. No explanation given.

https://github.com/coreutils/coreutils/blob/be3a216ab447d2f82daa3ff5851111856467e093/src/ioblksize.h

Feb 2024, best bufsize 256KiB. 128KiB OK, 64KiB too. 32KiB and less have
performance issues.
