{-# LANGUAGE CPP #-}
module System.Process.Kill (killProcessTree) where
import System.Process

#if !defined(WINDOWS)
import System.Process.Internals
import qualified System.Posix.Signals as Sig
import qualified System.Posix.Process as Proc
#endif

-- | Kill all processes in the same process group as the specified process.
killProcessTree :: ProcessHandle -> IO ()
killProcessTree ph =
#if defined(WINDOWS)
  -- terminateProcess should kill the process and its children.
  terminateProcess ph
#else
  withProcessHandle ph $ \ph_ -> case ph_ of
    OpenHandle pid -> sendKillSignal pid
    ClosedHandle {} -> return ()
#if MIN_VERSION_process(1, 5, 0)
    OpenExtHandle pid _ _ -> sendKillSignal pid
#endif
  where
    sendKillSignal pid = do
      pgid <- Proc.getProcessGroupIDOf pid
      Sig.signalProcessGroup Sig.killProcess pgid
#endif
