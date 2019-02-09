{-# LANGUAGE ForeignFunctionInterface #-}
{-# CFILES System/Posix/waitpid.c #-}

module System.Posix.Waitpid where

import Control.Monad
import Data.List
import Foreign
import Foreign.C
import Foreign.C.Types(CInt(..))
import System.Posix.Signals (Signal)
import System.Posix.Types (CPid(..))

foreign import ccall safe "SystemPosixWaitpid_waitpid" c_waitpid :: CPid -> Ptr CInt -> CInt -> IO CPid

data Flag = NoHang | IncludeUntraced | IncludeContinued deriving Show
data Status = Exited Int | Signaled Signal | Stopped Signal | Continued deriving (Show, Eq)

waitpid :: CPid -> [Flag] -> IO (Maybe (CPid, Status))
waitpid pid flags = alloca $ \resultPtr -> do
  child <- throwErrnoIfMinus1 "waitpid" $ c_waitpid pid resultPtr options
  result <- peek resultPtr
  return $ guard (child /= 0) >> return (child, extractResult result)
 where
  options = foldl' (.|.) 0 (map flagValue flags)
  flagValue NoHang = 1
  flagValue IncludeUntraced = 2
  flagValue IncludeContinued = 4
  extractResult res | res < 0x10000 = Exited (fromIntegral res)
                    | res < 0x20000 = Signaled (res - 0x10000)
                    | res < 0x30000 = Stopped (res - 0x20000)
                    | res == 0x30000 = Continued
                    | otherwise = error $ "waitpid: unexpected result " ++ show res
