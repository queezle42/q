{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Q.VT (
  runProbes,
  vtLockSwitch,
  vtUnlockSwitch,
) where

import Control.Concurrent
import Control.Monad.Catch
import Foreign
import Foreign.C
import Foreign.Ptr
import System.Posix.IO
import System.Posix.Types (Fd(..))
import Language.C.Inline qualified as C
import Quasar.Prelude

C.include "<sys/ioctl.h>"
C.include "<linux/kd.h>"
C.include "<linux/vt.h>"

vtPath :: FilePath
vtPath = "/dev/tty0"


vtLockSwitch :: IO ()
vtLockSwitch = withVT (ioctlDo [C.pure| unsigned long { VT_LOCKSWITCH } |])

vtUnlockSwitch :: IO ()
vtUnlockSwitch = withVT (ioctlDo [C.pure| unsigned long { VT_UNLOCKSWITCH } |])

withVT :: (Fd -> IO ()) -> IO ()
withVT action = withFd vtPath action

withFd :: FilePath -> (Fd -> IO ()) -> IO ()
withFd path = bracket aquire closeFd
  where
    aquire = openFd path ReadOnly Nothing defaultFileFlags{ nonBlock = True }

-- | ioctl without in/out parameter.
ioctlDo :: CULong -> Fd -> IO ()
ioctlDo request (Fd fd) = runInBoundThread do
  throwErrnoIfMinus1_ "ioctl" do
    [C.exp| int { ioctl($(int fd), $(unsigned long request)) } |]



-- | Check if a fd is a VT
isVT :: Fd -> IO Bool
isVT (Fd fd) = runInBoundThread do
  -- Linux always returns KB_101 if the fd is for a console/VT (see ioctl_console man-page)
  alloca \ptr -> do
    x <- [C.exp| int { ioctl($(int fd), KDGKBTYPE, $(char* ptr)) } |]
    if x < 0
      then do
        errno <- getErrno
        if errno == eNOTTY
          then pure False
          else throwErrno "ioctl"

      else (== [C.pure| char{ KB_101 } |]) <$> peek ptr



-- * Experiments


runProbes :: IO ()
runProbes = do
  mapM_ probePath [
    "/proc/self/fd/0",
    "/dev/tty",
    "/dev/tty0",
    "/dev/console"
    ]

probePath :: FilePath -> IO ()
probePath path = withFd path \fd -> do
  handle (\(ex :: IOError) -> traceIO (displayException ex)) do
    traceIO $ "probing for vt at " <> path
    vt <- isVT fd
    traceIO $ "- isVT: " <> show vt
    when vt do
      graphics <- graphicsMode fd
      traceIO $ "- graphics: " <> show graphics



-- | Check if a VT is running in graphics mode
graphicsMode :: Fd -> IO Bool
graphicsMode (Fd fd) = do
  result <- ioctlGetPtr (\ptr -> [C.exp| int { ioctl($(int fd), KDGETMODE, $(int* ptr)) } |])
  pure (result == [C.pure| int{ KD_GRAPHICS } |])


ioctlGetPtr :: Storable a => (Ptr a -> IO CInt) -> IO a
ioctlGetPtr action = runInBoundThread do
  alloca \ptr -> do
    resetErrno
    throwErrnoIfMinus1_ "ioctl" (action ptr)
    peek ptr
