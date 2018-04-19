{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
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
import Control.Monad
import Data.Foldable
import Data.Function
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe
import Data.Monoid
import System.Exit
import System.IO
import qualified Data.List.NonEmpty as NE
import qualified Data.Version as V
import qualified System.Timeout as Timeout
import Prelude

import Control.Concurrent.Async
import Control.Lens hiding ((.=))
import Data.Time
import Data.Time.Clock.POSIX
import Network.HTTP.Client (HttpException)
import Network.HTTP.Client.TLS
import Network.Socket
import System.FilePath ((</>))
import System.IO.Temp
import System.Process
import System.PosixCompat.User (getEffectiveUserName)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString.Lazy.Internal as BLI
import qualified Data.Text as T
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Network.HTTP.Types.Status as HT
import qualified Network.Socket.ByteString.Lazy as Socket
import qualified Network.Wreq as W
import qualified Options.Applicative as O

import System.Process.Kill (killProcessTree)
import qualified Paths_sensu_run as Paths

import Data.Aeson
#if MIN_VERSION_aeson(1, 2, 2)
  hiding (Options)
#endif

main :: IO ()
main = do
  issued <- getCurrentTime
  opts <- O.execParser $ O.info (O.helper <*> options) O.fullDesc
  case opts of
    ShowVersion -> do
      putStrLn $ "sensu-run " ++ V.showVersion Paths.version
      exitSuccess
    RunOptions {..} -> withSystemTempFile "sensu-run.XXX" $ \path hdl -> do
      executed <- getCurrentTime
      rawStatus <- try $ bracket
        (startProcess cmdspec)
        (\(_, _, ph) -> do
          terminateProcess ph
          killProcessTree ph
          waitForProcess ph)
        $ \(out, err, ph) -> do
          aout <- async $ redirectOutput out $ if redirect
            then [hdl, stdout] else [hdl]
          aerr <- async $ redirectOutput err $ if redirect
            then [hdl, stderr] else [hdl]
          mapM_ waitCatch [aout, aerr]
          withTimeout timeout $ waitForProcess ph
      hClose hdl
      exited <- getCurrentTime
      rawOutput <- BL.readFile path
      user <- T.pack <$> getEffectiveUserName
      let
        encoded = encode CheckResult
          { command = cmdspec
          , output = TL.toLazyText $ mconcat
            [ TL.fromLazyText (TL.decodeUtf8With TE.lenientDecode rawOutput)
            , case rawStatus of
              Left (ioe :: IOException) -> "\n" <> TL.fromString (show ioe)
              Right Nothing -> "\n" <> "sensu-run: timed out"
              Right _ -> mempty
            ]
          , status = case rawStatus of
            Right (Just ExitSuccess) -> OK
            Right (Just ExitFailure {}) -> CRITICAL
            _ -> UNKNOWN
          , duration = diffUTCTime exited executed
          , ..
          }
      if dryRun
        then BL8.putStrLn encoded
        else case endpoint of
          ClientSocketInput port -> sendToClientSocketInput port encoded
          SensuServer urls -> sendToSensuServer urls encoded
      case rawStatus of
        Left ioe -> do
          hPutStrLn stderr $ show ioe
          exitFailure
        Right (Just ExitSuccess) -> exitSuccess
        Right Nothing -> do
          hPutStrLn stderr $ showCmdSpec cmdspec ++ " timed out"
          exitFailure
        Right (Just ExitFailure {}) -> exitFailure

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
    params = W.defaults
      & W.header "Content-Type" .~ ["application/json"]
      & W.manager .~ Left tlsManagerSettings
    handleError reason = do
      hPutStrLn stderr $
        "Failed to POST results to Sensu server (" ++ reason ++ ")"
      exitFailure

startProcess :: CmdSpec -> IO (Handle, Handle, ProcessHandle)
startProcess cmdspec = do
  (_, Just out, Just err, ph) <- createProcess CreateProcess
    { cmdspec
    , cwd = Nothing
    , env = Nothing
    , std_in = Inherit
    , std_out = CreatePipe
    , std_err = CreatePipe
    , close_fds = False
    , create_group = True -- necessary to not kill sensu-run itself
    , delegate_ctlc = False
    , detach_console = False
    , create_new_console = False
    , new_session = False
    , child_group = Nothing
    , child_user = Nothing
#if MIN_VERSION_process(1, 5, 0)
    , use_process_jobs = True
#endif
    }
  return (out, err, ph)

redirectOutput :: Handle -> [Handle] -> IO ()
redirectOutput source sinks = fix $ \loop -> do
  eof <- hIsEOF source
  unless eof $ do
    chunk <- B.hGetSome source BLI.defaultChunkSize
    mapM_ (flip B.hPut chunk) sinks
    loop

withTimeout :: Maybe NominalDiffTime -> IO a -> IO (Maybe a)
withTimeout time io = case time of
  Just n -> Timeout.timeout (seconds n) io
  Nothing -> Just <$> io
  where
    seconds n = round $ n * 10 ^ (6 :: Int)

data Options
  = ShowVersion
  | RunOptions
    { name :: T.Text
    , cmdspec :: CmdSpec
    , source :: Maybe T.Text
    , ttl :: Maybe NominalDiffTime
    , timeout :: Maybe NominalDiffTime
    , handlers :: [T.Text]
    , endpoint :: Endpoint
    , redirect :: Bool
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
options = asum
  [ runOptions
  , ShowVersion <$ O.switch (O.long "version" <> O.short 'v')
  ]
  where
    runOptions = do
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
      redirect <- O.switch $ mconcat
        [ O.long "redirect"
        , O.help "Redirect command output to sensu-run's output"
        ]
      dryRun <- O.switch $ mconcat
        [ O.long "dry-run"
        , O.long "dry"
        , O.help "Dump the JSON object which is supposed to be sent"
        ]
      cmdspec <- cmdSpecOption
      return RunOptions {..}
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
  , user :: T.Text
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
    , "user" .= user
    ]
    where
      addOptional key val ps = maybe ps (\val' -> key .= val' : ps) val
      statusToInt ExitSuccess = 0
      statusToInt (ExitFailure n) = n

showCmdSpec :: CmdSpec -> String
showCmdSpec = \case
  ShellCommand cmd -> cmd
  RawCommand cmd args -> unwords $ cmd:args
