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

foreign import ccall safe "SystemPosixWaitpid_waitpid" c_waitpid :: CPid -> Ptr CInt -> Ptr CInt -> CInt -> IO CPid

-- Important to remember:
--     Don't confuse a program's exit status with a process' termination status.
--     There are lots of ways a process can terminate besides having its program
--     finish. In the event that the process termination is caused by program
--     termination (i.e., `exit`), though, the program's exit status becomes
--     part of the process' termination status.
-- Source:
--     https://www.gnu.org/software/libc/manual/html_node/Exit-Status.html#Exit-Status

data Flag = NoHang | IncludeUntraced | IncludeContinued deriving (Eq, Ord, Show)
data Status = Exited Int | Signaled Signal | Stopped Signal | Continued deriving (Eq, Ord, Show)
-- | Returning the full status is important for some use cases,
-- for example ptrace(), which sets higher bits of the status in
-- certain situations.
newtype FullStatus = FullStatus CInt deriving (Eq, Ord, Show)

waitpidFullStatus :: CPid -> [Flag] -> IO (Maybe (CPid, Status, FullStatus))
waitpidFullStatus pid flags = alloca $ \resultPtr -> alloca $ \fullStatusPtr -> do
  child <- throwErrnoIfMinus1 "waitpid" $ c_waitpid pid resultPtr fullStatusPtr options
  result <- peek resultPtr
  fullStatus <- peek fullStatusPtr
  return $ guard (child /= 0) >> return (child, extractResult result, FullStatus fullStatus)
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

waitpid :: CPid -> [Flag] -> IO (Maybe (CPid, Status))
waitpid pid flags = do
  let discardFullStatus (pid, status, _fullStatus) = (pid, status)
  fmap (fmap discardFullStatus) $ waitpidFullStatus pid flags
