{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
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
import Data.Foldable
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe
import System.Exit
import System.IO
import qualified Data.List.NonEmpty as NE
import qualified System.Timeout as Timeout

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Time
import Data.Time.Clock.POSIX
import Network.HTTP.Client (HttpException)
import Network.Socket
import System.FilePath ((</>))
import System.IO.Temp
import System.Process
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Network.HTTP.Types.Status as HT
import qualified Network.Socket.ByteString.Lazy as Socket
import qualified Network.Wreq as W
import qualified Options.Applicative as O

main :: IO ()
main = do
  issued <- getCurrentTime
  Options {..} <- O.execParser $ O.info (O.helper <*> options) O.fullDesc
  withSystemTempFile "sensu-run.XXX" $ \path hdl -> do
    executed <- getCurrentTime
    rawStatus <- bracket
      (startProcess cmdspec hdl)
      terminateProcess
      (withTimeout timeout . waitForProcess)
    exited <- getCurrentTime
    rawOutput <- BL.readFile path
    let
      encoded = encode CheckResult
        { command = cmdspec
        , output = TL.decodeUtf8With TE.lenientDecode rawOutput
        , status = case rawStatus of
          Nothing -> UNKNOWN
          Just ExitSuccess -> OK
          Just ExitFailure {} -> CRITICAL
        , duration = diffUTCTime exited executed
        , ..
        }
    if dryRun
      then BL8.putStrLn encoded
      else case endpoint of
        ClientSocketInput port -> sendToClientSocketInput port encoded
        SensuServer urls -> sendToSensuServer urls encoded
    case rawStatus of
      Just ExitSuccess -> exitSuccess
      Nothing -> do
        hPutStrLn stderr $ showCmdSpec cmdspec ++ " timed out"
        exitFailure
      Just ExitFailure {} -> exitFailure

sendToClientSocketInput
  :: PortNumber -- ^ Listening port of Sensu client socket
  -> BL8.ByteString -- ^ Payload
  -> IO ()
sendToClientSocketInput port payload = bracket open close $ \sock -> do
  localhost <- inet_addr "127.0.0.1"
  connect sock $ SockAddrInet port localhost
  Socket.sendAll sock payload
  `catch` \(ioe :: IOException) -> do
    hPutStrLn stderr $
      "Failed to write results to localhost:" ++ show port
        ++ " (" ++ show ioe ++ ")"
    exitFailure
  where
    open = socket AF_INET Stream defaultProtocol

sendToSensuServer
  :: NonEmpty String -- ^ Sensu server base URLs
  -> BL8.ByteString -- ^ Payload
  -> IO ()
sendToSensuServer urls payload =
  foldr go (handleError "no more retry") urls
    `catch` \(e :: HttpException) -> handleError (show e)
  where
    go url retry = do
      resp <- W.postWith params (url </> "results") payload
      let status = resp ^. W.responseStatus
      if
        | HT.statusIsClientError status -> handleError $ show status
        | HT.statusIsServerError status -> retry
        | HT.statusIsSuccessful status -> return ()
        | otherwise ->
          fail $ "sendToSensuServer: unexpected status " ++ show status
    params = W.defaults &
      W.header "Content-Type" .~ ["application/json"]
    handleError reason = do
      hPutStrLn stderr $
        "Failed to POST results to Sensu server (" ++ reason ++ ")"
      exitFailure

startProcess :: CmdSpec -> Handle -> IO ProcessHandle
startProcess cmdspec hdl = do
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
  return ph

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
  , endpoint :: Endpoint
  , dryRun :: Bool
  }

data Endpoint
  = ClientSocketInput PortNumber
  -- ^ Local client socket input
  | SensuServer (NonEmpty String)
  -- ^ Sensu server API or a client HTTP socket
  --
  -- Multiple HTTP endpoints can be specified. sensu-run retries sequentially
  -- until it succeeds. By default Sensu servers listen on port 4567 and
  -- client HTTP sockets listen on 3031.

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
  endpoint <- asum
    [ ClientSocketInput <$> portOption
    , SensuServer . NE.fromList <$> O.some serverOption
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
    portOption = O.option O.auto $ mconcat
      [ O.long "port"
      , O.metavar "PORT"
      , O.help
        "Send results to the local sensu-client listening on the specified port"
      , O.showDefault
      , O.value 3030
      ]
    serverOption = O.strOption $ mconcat
      [ O.long "server"
      , O.metavar "URL"
      , O.help "Send results to the specified Sensu server"
      ]

data CheckResult = CheckResult
  { name :: T.Text
  , command :: CmdSpec
  , status :: ExitCode
  , source :: Maybe T.Text
  , issued :: UTCTime
  , executed :: UTCTime
  , duration :: NominalDiffTime
  , output :: TL.Text
  , handlers :: [T.Text]
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
    , "duration" .= (realToFrac duration :: Double)
    , "status" .= statusToInt status
    , "output" .= output
    , "handlers" .= handlers
    ]
    where
      addOptional key val ps = maybe ps (\val' -> key .= val' : ps) val
      statusToInt ExitSuccess = 0
      statusToInt (ExitFailure n) = n

showCmdSpec :: CmdSpec -> String
showCmdSpec = \case
  ShellCommand cmd -> cmd
  RawCommand cmd args -> unwords $ cmd:args
