{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Main where
import Control.Exception
import Data.Maybe
import System.Exit
import System.IO
import qualified System.Timeout as Timeout

import Data.Aeson
import Data.Time
import Data.Time.Clock.POSIX
import Network.Socket
import System.IO.Temp
import System.Process
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Network.Socket.ByteString.Lazy as Socket
import qualified Options.Applicative as O

main :: IO ()
main = do
  issued <- getCurrentTime
  Options {..} <- O.execParser $ O.info (O.helper <*> options) O.fullDesc
  withSystemTempFile "sensu-run.XXX" $ \path hdl -> do
    executed <- getCurrentTime
    (_, _, _, ph) <- createProcess CreateProcess
      { cmdspec
      , cwd = Nothing
      , env = Nothing
      , std_in = Inherit
      , std_out = UseHandle hdl
      , std_err = UseHandle hdl
      , close_fds = False
      , create_group = False
      , delegate_ctlc = False
      , detach_console = False
      , create_new_console = False
      , new_session = False
      , child_group = Nothing
      , child_user = Nothing
      }
    rawStatus <- withTimeout timeout $ waitForProcess ph
    exited <- getCurrentTime
    terminateProcess ph
    bracket
      (socket AF_INET Stream defaultProtocol)
      close
      $ \sock -> do
        rawOutput <- BL.readFile path
        let
          encoded = encode $ CheckResult
            { command = cmdspec
            , duration = diffUTCTime exited executed
            , output = TL.decodeUtf8With TE.lenientDecode rawOutput
            , status = case rawStatus of
              Nothing -> UNKNOWN
              Just ExitSuccess -> OK
              Just (ExitFailure {}) -> CRITICAL
            , ..
            }
        if dryRun
          then BL8.putStrLn encoded
          else do
            localhost <- inet_addr "127.0.0.1"
            connect sock $ SockAddrInet port localhost
            Socket.sendAll sock encoded
        `catch` \(ioe :: IOException) -> do
          hPutStrLn stderr $
            "Failed to write results to localhost:3030 (" ++ show ioe ++ ")"
          exitFailure
    case rawStatus of
      Just ExitSuccess -> exitSuccess
      Nothing -> do
        hPutStrLn stderr $ showCmdSpec cmdspec ++ " timed out"
        exitFailure
      Just (ExitFailure {}) -> exitFailure

withTimeout :: Maybe NominalDiffTime -> IO a -> IO (Maybe a)
withTimeout time io = case time of
  Just n -> Timeout.timeout (round $ n * 10^(6 :: Int))  io
  Nothing -> Just <$> io

data Options = Options
  { name :: T.Text
  , cmdspec :: CmdSpec
  , source :: Maybe T.Text
  , ttl :: Maybe NominalDiffTime
  , timeout :: Maybe NominalDiffTime
  , handlers :: [T.Text]
  , port :: PortNumber
  , dryRun :: Bool
  }

options :: O.Parser Options
options = do
  name <- textOption $ mconcat
    [ O.short 'n'
    , O.long "name"
    , O.metavar "NAME"
    , O.help "The name of the check"
    ]
  source <- O.optional $ textOption $ mconcat
    [ O.long "source"
    , O.metavar "SOURCE"
    , O.help $ unlines
      [ "The check source, used to create a JIT Sensu client for an"
      , "external resource" ]
    ]
  ttl <- durationOption $ mconcat
    [ O.long "ttl"
    , O.metavar "SECONDS"
    , O.help "The time to live in seconds until check results are considered stale"
    ]
  timeout <- durationOption $ mconcat
    [ O.long "timeout"
    , O.metavar "SECONDS"
    , O.help "The check executaion duration timeout in seconds"
    ]
  handlers <- O.some $ textOption $ mconcat
    [ O.long "handler"
    , O.metavar "HANDLER"
    , O.help "Sensu event handler(s) to use for events created by the check"
    ]
  port <- O.option O.auto $ mconcat
    [ O.long "port"
    , O.metavar "PORT"
    , O.help "Port number that sensu-client is listening on"
    , O.showDefault
    , O.value 3030
    ]
  dryRun <- O.switch $ mconcat
    [ O.long "dry-run"
    , O.long "dry"
    ]
  cmdspec <- cmdSpecOption
  return Options {..}
  where
    textOption m = T.pack <$> O.strOption m
    durationOption m =
      fmap (realToFrac @Double) <$> O.optional (O.option O.auto m)
    cmdSpecOption = cmdSpec
      <$> O.optional
        (O.switch $ mconcat
          [ O.short 's'
          , O.long "shell"
          , O.help "Execute the command using the shell"
          ])
      <*> O.some (O.strArgument $ O.metavar "COMMAND")
      where
        cmdSpec (fromMaybe False -> isShell) args
          | isShell = ShellCommand (unwords args)
          | otherwise = RawCommand (head args) (tail args)

data CheckResult = CheckResult
  { name :: T.Text
  , command :: CmdSpec
  , status :: ExitCode
  , source :: Maybe T.Text
  , issued :: UTCTime
  , executed :: UTCTime
  , duration :: NominalDiffTime
  , output :: TL.Text
  }

pattern OK :: ExitCode
pattern OK = ExitSuccess

pattern WARNING :: ExitCode
pattern WARNING = ExitFailure 1

pattern CRITICAL :: ExitCode
pattern CRITICAL = ExitFailure 2

pattern UNKNOWN :: ExitCode
pattern UNKNOWN = ExitFailure 3

instance ToJSON CheckResult where
  toJSON = object . checkResultKeyValue
  toEncoding = pairs . mconcat . checkResultKeyValue

checkResultKeyValue :: KeyValue a => CheckResult -> [a]
checkResultKeyValue CheckResult {..} =
  addOptional "source" source
    [ "name" .= name
    , "command" .= showCmdSpec command
    , "issued" .= (floor (utcTimeToPOSIXSeconds issued) :: Int)
    , "executed" .= (floor (utcTimeToPOSIXSeconds executed) :: Int)
    , "duration" .= (round duration :: Int)
    , "status" .= statusToInt status
    , "output" .= output
    ]
    where
      addOptional key val ps = maybe ps (\val' -> key .= val' : ps) val
      statusToInt ExitSuccess = 0
      statusToInt (ExitFailure n) = n

showCmdSpec :: CmdSpec -> String
showCmdSpec = \case
  ShellCommand cmd -> cmd
  RawCommand cmd args -> unwords $ cmd:args
