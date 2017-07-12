{-# LANGUAGE CPP #-}
module System.Process.Kill (killProcess) where
import System.Process

#if !defined(WINDOWS)
import System.Process.Internals
import qualified System.Posix.Signals as Sig
#endif

killProcess :: ProcessHandle -> IO ()
killProcess ph =
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
    sendKillSignal = Sig.signalProcess Sig.killProcess
#endif
