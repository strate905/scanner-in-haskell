{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Main
Description : Educational Network Port Scanner
License     : GPLv3

This is an educational network port scanner written in Haskell to demonstrate:
- Concurrent programming with async and semaphores
- Network programming with sockets (TCP connect() and UDP probes)
- Error handling patterns (Maybe, Either, try, timeout)
- Command-line parsing and JSON serialization
- Resource management with bracket
- Rate limiting and retry logic

The scanner uses user-space techniques (no raw sockets) so it runs without
special privileges. It's designed for clarity and learning, not performance.

Key educational concepts demonstrated:
- Algebraic data types (sum types, product types)
- Type classes (ToJSON, Show, Eq, Ord)
- Higher-order functions (map, filter, traverse, sortOn)
- Do notation for different monads (IO, Either, MVar operations)
- Pattern matching and guards
- List comprehensions
- Bit manipulation for network address formatting
-}

module Main (main) where

import           Control.Applicative        (optional, (<**>))
import           Control.Concurrent         (threadDelay)
import           Control.Concurrent.Async   (mapConcurrently)
import           Control.Concurrent.MVar    (MVar, modifyMVar, newMVar)
import           Control.Concurrent.QSem    (QSem, newQSem, signalQSem,
                                             waitQSem)
import           Control.Exception          (IOException, bracket, bracket_,
                                             try)
import           Control.Monad              (when)
import           Data.Aeson                 (ToJSON (..), Value (..), object,
                                             (.=))
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Bits                  ((.&.), shiftR)
import           Data.Char                  (isSpace, toLower)
import           Data.List                  (intercalate, nub, sort, sortOn)
import           Data.Maybe                 (fromMaybe)
import           Data.Time.Clock            (NominalDiffTime, UTCTime,
                                             addUTCTime, diffUTCTime,
                                             getCurrentTime)
import           Data.Word                  (Word16, Word32)
import           Foreign.C.Error            (Errno (..), eCONNREFUSED,
                                             eHOSTDOWN, eHOSTUNREACH,
                                             eNETUNREACH)
import qualified GHC.IO.Exception           as GIOE
import           Network.Socket             (AddrInfo (..), AddrInfoFlag (..),
                                             Family (..), SockAddr (..),
                                             Socket, SocketOption (NoDelay),
                                             SocketType (..), PortNumber,
                                             addrAddress, addrFamily, close,
                                             connect, defaultHints,
                                             defaultProtocol, getAddrInfo,
                                             hostAddress6ToTuple,
                                             hostAddressToTuple, setSocketOption,
                                             socket, withSocketsDo)
import qualified Network.Socket.ByteString  as NBS
import           Options.Applicative        (Parser, ParserInfo, eitherReader,
                                             execParser, fullDesc, header, help,
                                             helper, info, long, metavar,
                                             option, progDesc, showDefault,
                                             strArgument, switch, value)
import qualified Options.Applicative        as Opt
import           System.Exit                (die)
import           System.IO.Error            (ioeGetErrorString)
import           System.Timeout             (timeout)
import           Text.Printf                (printf)
import           Text.Read                  (readMaybe)

--------------------------------------------------------------------------------
-- Options and configuration
-- Educational note: We use record syntax to define our configuration type.
-- This provides named field access and clear documentation of each setting.

data Options = Options
  { optTcpPorts    :: Maybe [Int]
  , optUdpPorts    :: Maybe [Int]
  , optTimeout     :: Double
  , optRetries     :: Int
  , optConcurrency :: Int
  , optRate        :: Maybe Int
  , optNoDNS       :: Bool
  , optJSON        :: Bool
  , optTarget      :: String
  }
  deriving (Show)

-- Educational note: Sum types (also called tagged unions or algebraic data types)
-- let us represent a value that can be one of several distinct alternatives.
-- Here Protocol can only be TCP or UDP, making illegal states unrepresentable.
data Protocol = ProtoTCP | ProtoUDP
  deriving (Eq, Ord, Show)  -- Derive standard type classes for comparison and display

protocolLabel :: Protocol -> String
protocolLabel ProtoTCP = "TCP"
protocolLabel ProtoUDP = "UDP"

data ScanResult = ScanResult
  { srPort     :: Int
  , srProtocol :: Protocol
  , srState    :: String
  , srService  :: String
  , srReason   :: String
  }
  deriving (Show)

-- Educational note: Type class instances define how types behave.
-- ToJSON instance explains how to serialize a ScanResult to JSON format.
-- The RecordWildCards extension lets us use {..} to automatically extract all fields.
instance ToJSON ScanResult where
  toJSON ScanResult{..} =
    object
      [ "port" .= srPort
      , "protocol" .= case srProtocol of
          ProtoTCP -> "tcp" :: String  -- Convert sum type to JSON-friendly string
          ProtoUDP -> "udp" :: String
      , "state" .= srState
      , "service" .= srService
      , "reason" .= srReason
      ]

data TargetDetails = TargetDetails
  { tdInput    :: String
  , tdResolved :: String
  , tdAddrInfo :: AddrInfo
  }

--------------------------------------------------------------------------------
-- Rate limiting helper
-- Educational note: Rate limiting prevents overwhelming networks or hosts.
-- We use an MVar (mutable variable) to coordinate timing across concurrent threads.
-- The '!' marks (strict fields) ensure values are evaluated immediately, preventing space leaks.

data RateLimiter
  = NoRate  -- Rate limiting disabled (unlimited speed)
  | RateLimiter !NominalDiffTime !(MVar UTCTime) -- interval between probes + next allowed time

-- Educational note: Constructor function that creates a rate limiter.
-- Pattern matching on Maybe handles both "no limit" and "specific rate" cases.
-- Guards (|) provide conditional matching within a single pattern.
newRateLimiter :: Maybe Int -> IO RateLimiter
newRateLimiter Nothing           = pure NoRate  -- No rate specified = no limiting
newRateLimiter (Just r) | r <= 0 = pure NoRate  -- Zero/negative rate = no limiting
newRateLimiter (Just r) = do
  now <- getCurrentTime           -- Get current wall-clock time
  mv  <- newMVar now              -- Create mutable variable to track next allowed probe time
  let interval = realToFrac (1 / fromIntegral r) :: NominalDiffTime  -- Convert rate (probes/sec) to interval (seconds/probe)
  pure (RateLimiter interval mv)

-- Educational note: Before sending each probe, threads call this to respect the rate limit.
-- modifyMVar provides atomic read-modify-write on the shared schedule, preventing race conditions.
acquireRate :: RateLimiter -> IO ()
acquireRate NoRate = pure ()  -- No limiting: return immediately
acquireRate (RateLimiter interval mv) =
  -- Guarded section enforces a global cadence even across many async workers.
  -- modifyMVar locks the MVar during the entire operation, ensuring thread-safe updates.
  modifyMVar mv $ \nextAllowed -> do
    now <- getCurrentTime
    let target  = max nextAllowed now      -- Wait until at least 'nextAllowed' or now (whichever is later)
        waitFor = diffUTCTime target now   -- Calculate how long to sleep
    when (waitFor > 0) $                   -- Only delay if we're ahead of schedule
      threadDelay (ceiling (waitFor * 1000000))  -- threadDelay takes microseconds
    let newNext = addUTCTime interval target     -- Schedule next probe one interval later
    pure (newNext, ())  -- Return new MVar value and unit result

--------------------------------------------------------------------------------
-- Main
-- Educational note: The entry point orchestrates all scanner components.
-- withSocketsDo ensures proper initialization/cleanup of network subsystem.

main :: IO ()
main = withSocketsDo $ do  -- Windows requires socket subsystem initialization
  options <- execParser optsParser  -- Parse command-line arguments
  -- Use provided ports or fall back to sensible defaults
  let tcpPorts = fromMaybe defaultTcpPorts (optTcpPorts options)
      udpPorts = fromMaybe defaultUdpPorts (optUdpPorts options)

  -- Validate that user selected at least one port to scan
  when (null tcpPorts && null udpPorts) $
    die "No ports selected to scan (use --tcp or --udp)."

  targetDetails <- resolveTarget (optNoDNS options) (optTarget options)
  rateLimiter   <- newRateLimiter (optRate options)
  -- QSem (quantity semaphore) limits concurrent operations to prevent resource exhaustion
  semaphore     <- newQSem (max 1 (optConcurrency options))

  let timeoutMicros = toTimeoutMicros (optTimeout options)
      retries       = max 0 (optRetries options)
      -- Educational note: List comprehensions build task lists for each port.
      -- Wrapping each scan with 'withSem' ensures the semaphore controls concurrency.
      tcpTasks      =
        [ withSem semaphore (scanTcp targetDetails rateLimiter timeoutMicros retries port)
        | port <- tcpPorts  -- Generate one task per TCP port
        ]
      udpTasks      =
        [ withSem semaphore (scanUdp targetDetails rateLimiter timeoutMicros retries port)
        | port <- udpPorts  -- Generate one task per UDP port
        ]

  -- Educational note: mapConcurrently spawns lightweight threads for each task,
  -- running them in parallel and collecting all results. This is safe because
  -- each task respects the semaphore (limits parallelism) and rate limiter (paces probes).
  results <- mapConcurrently id (tcpTasks ++ udpTasks)
  -- Sort results by protocol first (TCP before UDP), then by port number
  let ordered = sortOn (\r -> (protocolOrder (srProtocol r), srPort r)) results

  -- Output results in requested format
  if optJSON options
    then BL.putStrLn (Aeson.encode (jsonReport targetDetails ordered))
    else renderTable targetDetails ordered

--------------------------------------------------------------------------------
-- CLI parsing

optsParser :: ParserInfo Options
optsParser =
  info (helper <*> parser) $
       fullDesc
    <> progDesc "Educational port scanner using TCP connect() and UDP probes. UDP is tricky: many hosts drop probes so silence often maps to open|filtered."
    <> header "scanner - small, readable network scanner"
  where
    parser :: Parser Options
    parser =
      Options
        <$> optional (option (eitherReader parsePortSpec)
              ( long "tcp"
             <> metavar "PORTS"
             <> help "Comma list or ranges of TCP ports (use 'none' to disable)." ))
        <*> optional (option (eitherReader parsePortSpec)
              ( long "udp"
             <> metavar "PORTS"
             <> help "Comma list or ranges of UDP ports (use 'none' to disable)." ))
        <*> option Opt.auto
              ( long "timeout"
             <> metavar "SECONDS"
             <> help "Timeout per probe attempt."
             <> showDefault
             <> value 3.0 )
        <*> option Opt.auto
              ( long "retries"
             <> metavar "N"
             <> help "Additional retries for inconclusive results."
             <> showDefault
             <> value 1 )
        <*> option Opt.auto
              ( long "concurrency"
             <> metavar "N"
             <> help "Maximum concurrent probes."
             <> showDefault
             <> value 64 )
        <*> optional (option Opt.auto
              ( long "rate"
             <> metavar "PPS"
             <> help "Global rate limit (probes per second)." ))
        <*> switch
              ( long "no-dns"
             <> help "Disable DNS resolution (target must be numeric)." )
        <*> switch
              ( long "json"
             <> help "Emit JSON instead of Markdown table." )
        <*> fmap (fromMaybe "127.0.0.1")
              (optional (strArgument
                ( metavar "target"
               <> help "Target hostname or IP (default 127.0.0.1)." )))

--------------------------------------------------------------------------------
-- Port specifications
-- Educational note: This parser converts user input like "80,443,8000-8010" into [Int].
-- Either String [Int] means: Left String for errors, Right [Int] for success.
-- This is a common Haskell pattern for functions that can fail with useful error messages.

parsePortSpec :: String -> Either String [Int]
parsePortSpec rawInput
  | null trimmed = Left "Port list cannot be empty."
  | map toLower trimmed == "none" = Right []  -- Special keyword to disable a protocol
  | otherwise = do
      -- Educational note: 'do' notation works with any Monad, including Either.
      -- Here it provides early-exit error handling: if any parseEntry fails (Left),
      -- the whole function immediately returns that error.
      entries <- traverse parseEntry pieces  -- Parse each piece, collect as Either String [[Int]]
      pure . sort . nub . concat $ entries   -- Flatten, remove duplicates, sort
  where
    trimmed = trim rawInput
    pieces  = filter (not . null) (map trim (splitOnComma trimmed))  -- Split on commas, trim whitespace

    parseEntry entry =
      -- Educational note: 'break' splits a string at the first character matching a predicate.
      -- We use it to detect ranges (e.g., "8000-8010") vs single ports (e.g., "80").
      case break (== '-') entry of
        (_, "") -> parseSingle entry        -- No dash found: treat as single port
        (a, '-':b) -> expandRange a b      -- Found dash: parse as range "a-b"
        _ -> Left ("Invalid port entry: " ++ entry)

    parseSingle str =
      case readMaybe str of
        Nothing -> Left ("Invalid port number: " ++ str)
        Just p  -> validatePort p >> pure [p]

    expandRange a b = do
      start <- maybe (Left ("Invalid port number: " ++ a)) Right (readMaybe a)
      end   <- maybe (Left ("Invalid port number: " ++ b)) Right (readMaybe b)
      when (start > end) $
        Left ("Port range start greater than end: " ++ a ++ "-" ++ b)
      validatePort start
      validatePort end
      pure [start .. end]

    validatePort p
      | p < 1 || p > 65535 = Left ("Port out of range (1-65535): " ++ show p)
      | otherwise          = Right ()

    trim = dropWhile isSpace . dropWhileEnd isSpace
    dropWhileEnd f = reverse . dropWhile f . reverse

    splitOnComma [] = [""]
    splitOnComma (c:cs)
      | c == ','  = "" : rest
      | otherwise = (c : head rest) : tail rest
      where
        rest = splitOnComma cs

--------------------------------------------------------------------------------
-- Defaults

defaultTcpPorts :: [Int]
defaultTcpPorts =
  [ 80, 443, 22, 21, 25, 23, 53, 110, 135, 139, 143, 445, 3389, 3306
  , 8080, 5900, 993, 995, 465, 587, 111, 2049, 1025, 1723, 554
  ]

defaultUdpPorts :: [Int]
defaultUdpPorts = [53, 123, 161, 500, 1900]

--------------------------------------------------------------------------------
-- Target resolution

resolveTarget :: Bool -> String -> IO TargetDetails
resolveTarget noDns inputHost = do
  let hints =
        defaultHints
          { addrSocketType = Stream
          , addrFlags = if noDns
              then [AI_NUMERICHOST]
              else [AI_ADDRCONFIG, AI_V4MAPPED]
          }
  infos <- getAddrInfo (Just hints) (Just inputHost) Nothing
  case pickPreferred infos of
    Nothing ->
      die "Unable to resolve target host."
    Just info -> do
      let resolved = formatSockAddr (addrAddress info)
      pure TargetDetails
            { tdInput = inputHost
            , tdResolved = resolved
            , tdAddrInfo = info
            }
  where
    pickPreferred [] = Nothing
    pickPreferred xs =
      -- Prefer IPv4 when available to keep examples simpler on dual-stack hosts.
      Just (head (sortOn preference xs))

    preference ai =
      case addrFamily ai of
        AF_INET  -> (0 :: Int)
        AF_INET6 -> 1
        _        -> 2

--------------------------------------------------------------------------------
-- Scanning orchestration
-- Educational note: These functions coordinate probe execution with proper
-- resource management (semaphores, rate limiting, retries, timeouts).

withSem :: QSem -> IO a -> IO a
withSem sem action =
  -- Educational note: bracket_ ensures cleanup code runs even if exceptions occur.
  -- We acquire a semaphore token (waitQSem), run the action, then release (signalQSem).
  -- This pattern prevents resource leaks when threads fail unexpectedly.
  bracket_ (waitQSem sem) (signalQSem sem) action

scanTcp
  :: TargetDetails
  -> RateLimiter
  -> Int
  -> Int
  -> Int
  -> IO ScanResult
scanTcp target rateLimiter timeoutMicros retries port =
  scanWithRetries ProtoTCP retries (tcpAttempt target rateLimiter timeoutMicros port)

scanUdp
  :: TargetDetails
  -> RateLimiter
  -> Int
  -> Int
  -> Int
  -> IO ScanResult
scanUdp target rateLimiter timeoutMicros retries port =
  scanWithRetries ProtoUDP retries (udpAttempt target rateLimiter timeoutMicros port)

-- Educational note: Retry logic for unreliable network probes.
-- Some states ("filtered", "open|filtered") are inconclusive; we retry those.
-- Definitive states ("open", "closed") are returned immediately.
scanWithRetries
  :: Protocol
  -> Int           -- Number of retries (in addition to the initial attempt)
  -> IO ScanResult -- The probe action to execute (may be run multiple times)
  -> IO ScanResult
scanWithRetries proto retries attempt = go (retries + 1) Nothing
  where
    -- Recursive helper with attempt counter and last result
    go 0 (Just result) = pure result  -- Out of attempts: return last result
    go 0 Nothing       = pure fallback  -- No attempts ever ran (shouldn't happen)
    go n previous = do
      -- Retry inconclusive states while we still have budgeted attempts.
      result <- attempt
      if srState result `elem` finalStates
        then pure result  -- Definitive answer: return immediately without retrying
        else
          if n > 1
            then go (n - 1) (Just result)  -- Still have retries: try again
            else pure result               -- Last attempt: accept whatever we got

    finalStates = ["open", "closed"]  -- These states are conclusive, don't retry
    fallback =  -- Fallback result (should never be reached in practice)
      ScanResult
        { srPort = 0
        , srProtocol = proto
        , srState = "error"
        , srService = "?"
        , srReason = "No attempts executed."
        }

-- Educational note: TCP scanning uses the connect() system call (a "full connection").
-- This is reliable but easily detected. It's chosen here for educational clarity and
-- because it doesn't require raw sockets or special privileges.
tcpAttempt :: TargetDetails -> RateLimiter -> Int -> Int -> IO ScanResult
tcpAttempt TargetDetails{..} rateLimiter timeoutMicros port = do
  let baseAddr = addrAddress tdAddrInfo
      sockAddr = setPort baseAddr (fromIntegral port)  -- Insert target port into address
      family   = addrFamily tdAddrInfo  -- AF_INET (IPv4) or AF_INET6 (IPv6)
  acquireRate rateLimiter  -- Respect rate limit before sending probe
  -- Educational note: timeout wraps an IO action with a time limit.
  -- If the action doesn't complete in time, timeout returns Nothing (no exception).
  outcome <- timeout timeoutMicros $
    -- bracket ensures socket cleanup even if connect() throws an exception
    bracket (socket family Stream defaultProtocol) close $ \sock -> do
      setSocketOption sock NoDelay 1  -- Disable Nagle's algorithm for faster response
      try (connect sock sockAddr)  -- try catches IOErrors and wraps them in Either
  -- Educational note: Pattern matching on nested Maybe and Either extracts the result.
  case outcome of
    Nothing ->  -- timeout expired: host didn't respond
      pure baseResult { srState = "filtered", srReason = "connect() timed out" }
    Just (Left err) ->  -- connect() returned an error (refused, unreachable, etc.)
      pure (classifyTcpError baseResult err)
    Just (Right _) ->  -- connect() succeeded: port is open
      pure baseResult { srState = "open", srReason = "connect() succeeded" }
  where
    baseResult =  -- Template result filled in by each case above
      ScanResult
        { srPort = port
        , srProtocol = ProtoTCP
        , srState = "filtered"
        , srService = serviceGuess ProtoTCP port
        , srReason = "initial"
        }

-- Educational note: UDP scanning is inherently ambiguous because UDP has no handshake.
-- We send a packet and listen for a response. Three outcomes:
-- 1. Reply received → port is open
-- 2. ICMP "Port Unreachable" → port is closed
-- 3. Silence → could be open (service ignoring us) or filtered (firewall dropping packets)
udpAttempt :: TargetDetails -> RateLimiter -> Int -> Int -> IO ScanResult
udpAttempt TargetDetails{..} rateLimiter timeoutMicros port = do
  let baseAddr = addrAddress tdAddrInfo
      sockAddr = setPort baseAddr (fromIntegral port)
      family   = addrFamily tdAddrInfo
      payload  = BS.pack "hi" -- tiny probe that fits most services without upsetting them
  acquireRate rateLimiter
  bracket (socket family Datagram defaultProtocol) close $ \sock -> do
    connect sock sockAddr  -- "connect" on UDP socket sets default destination (no handshake)
    sendOutcome <- try (NBS.send sock payload)
    case sendOutcome of
      Left err ->
        pure baseResult { srState = "filtered", srReason = "send failed: " ++ friendlyError err }
      Right _ -> do
        -- Educational note: We use timeout and try together to handle both time limits
        -- and exceptions. The type becomes: Maybe (Either IOError ByteString).
        recvOutcome <- timeout timeoutMicros (try (NBS.recv sock 512))
        case recvOutcome of
          Nothing ->
            -- Educational note: Silence is the most common UDP response.
            -- Many services intentionally don't reply to unknown probes.
            -- Firewalls may also silently drop packets, so we can't distinguish
            -- between "open but silent" and "filtered by firewall".
            pure baseResult { srState = "open|filtered", srReason = "no reply (no ICMP)" }
          Just (Right bs)
            | BS.null bs ->  -- Empty datagram (unusual but valid)
                pure baseResult { srState = "open", srReason = "empty UDP response" }
            | otherwise ->   -- Received actual data: definitely open
                pure baseResult { srState = "open", srReason = "UDP reply received" }
          Just (Left err) ->
            -- Educational note: ICMP errors are reported as recv() errors.
            -- ECONNREFUSED corresponds to ICMP "Port Unreachable" (type 3, code 3).
            pure (classifyUdpError baseResult err)
  where
    baseResult =
      ScanResult
        { srPort = port
        , srProtocol = ProtoUDP
        , srState = "open|filtered"
        , srService = serviceGuess ProtoUDP port
        , srReason = "pending"
        }

--------------------------------------------------------------------------------
-- Socket helpers
-- Educational note: These utility functions handle low-level socket address manipulation.

-- Educational note: SockAddr represents network addresses (IPv4, IPv6, Unix sockets, etc.).
-- This function replaces the port number while preserving all other address components.
setPort :: SockAddr -> PortNumber -> SockAddr
setPort (SockAddrInet _ host) port = SockAddrInet port host  -- IPv4: replace port, keep 32-bit host address
setPort (SockAddrInet6 _ flow host scope) port =
  SockAddrInet6 port flow host scope  -- IPv6: replace port, keep flow, 128-bit host, scope
setPort other _ = other  -- Other address families (Unix sockets, etc.): return unchanged

--------------------------------------------------------------------------------
-- Error classification
-- Educational note: The OS reports different errors for different network conditions.
-- We map these errno codes to human-readable scan states and explanations.

-- Educational note: TCP errors tell us whether a port is closed or filtered.
-- ECONNREFUSED means the host received our SYN and replied with RST (port closed).
-- EHOSTUNREACH/ENETUNREACH mean routers couldn't reach the host (filtered).
classifyTcpError :: ScanResult -> IOError -> ScanResult
classifyTcpError base err =
  case errnoOf err of
    Just errno
      | errno == eCONNREFUSED ->  -- Host actively refused connection (RST packet)
          base { srState = "closed", srReason = "RST after SYN/ACK attempt" }
      | errno == eHOSTUNREACH || errno == eNETUNREACH || errno == eHOSTDOWN ->
          base { srState = "filtered", srReason = "unreachable host or network" }
    _ ->  -- Other errors (permission denied, etc.)
      base { srState = "filtered", srReason = "connect failed: " ++ friendlyError err }

-- Educational note: UDP error classification is similar but has key differences.
-- ECONNREFUSED on UDP means the OS received ICMP "Port Unreachable" (type 3, code 3).
-- This is the ONLY reliable way to confirm a UDP port is closed.
classifyUdpError :: ScanResult -> IOError -> ScanResult
classifyUdpError base err =
  case errnoOf err of
    Just errno
      | errno == eCONNREFUSED ->  -- Kernel received ICMP Port Unreachable
          base { srState = "closed", srReason = "ICMP 3/3 Port Unreachable" }
      | errno == eHOSTUNREACH || errno == eNETUNREACH || errno == eHOSTDOWN ->
          base { srState = "filtered", srReason = "unreachable host or network" }
    _ ->
      base { srState = "filtered", srReason = "recv failed: " ++ friendlyError err }

-- Educational note: Extract the numeric errno from a Haskell IOError.
-- This lets us compare against platform error codes (ECONNREFUSED, etc.).
errnoOf :: IOError -> Maybe Errno
errnoOf err = Errno <$> GIOE.ioe_errno err

friendlyError :: IOError -> String
friendlyError = ioeGetErrorString

--------------------------------------------------------------------------------
-- Output

jsonReport :: TargetDetails -> [ScanResult] -> Value
jsonReport TargetDetails{..} rows =
  object
    [ "target_input" .= tdInput
    , "resolved_ip" .= tdResolved
    , "results" .= rows
    ]

renderTable :: TargetDetails -> [ScanResult] -> IO ()
renderTable TargetDetails{..} rows = do
  putStrLn ("Target: " ++ tdInput ++ " (" ++ tdResolved ++ ")")
  putStrLn "| Port | Proto | State        | Service    | Reason                     |"
  putStrLn "|-----:|:-----:|:-------------|:----------|:---------------------------|"
  mapM_ printRow rows
  putStrLn ""
  putStrLn "Service column is a best-effort guess from IANA registrations; verify before trusting."
  putStrLn "UDP scanning is tricky: silence often means open|filtered because many hosts drop probes."
  where
    printRow ScanResult{..} =
      putStrLn $
        printf "| %4d | %5s | %-13s | %-10s | %-25s |"
          srPort (protocolLabel srProtocol) srState srService srReason

--------------------------------------------------------------------------------
-- Service guesses

serviceGuess :: Protocol -> Int -> String
serviceGuess proto port =
  case lookup port mapping of
    Just name -> name
    Nothing   -> "?"
  where
    mapping =
      case proto of
        ProtoTCP -> tcpServices
        ProtoUDP -> udpServices

tcpServices :: [(Int, String)]
tcpServices =
  [ (21, "ftp"), (22, "ssh"), (23, "telnet"), (25, "smtp"), (53, "dns")
  , (80, "http"), (110, "pop3"), (135, "msrpc"), (139, "netbios")
  , (143, "imap"), (443, "https"), (445, "smb"), (465, "smtps")
  , (587, "submission"), (993, "imaps"), (995, "pop3s"), (3306, "mysql")
  , (3389, "rdp"), (5900, "vnc"), (8080, "http-alt"), (111, "rpcbind")
  , (2049, "nfs"), (1723, "pptp"), (554, "rtsp"), (1025, "listener")
  ]

udpServices :: [(Int, String)]
udpServices =
  [ (53, "dns"), (123, "ntp"), (161, "snmp"), (500, "isakmp"), (1900, "ssdp") ]

--------------------------------------------------------------------------------
-- Utilities

protocolOrder :: Protocol -> Int
protocolOrder ProtoTCP = 0
protocolOrder ProtoUDP = 1

toTimeoutMicros :: Double -> Int
toTimeoutMicros seconds =
  max 1000 (floor (seconds * 1000000))

-- Educational note: Convert socket addresses to human-readable strings.
-- This demonstrates bit manipulation and network address formatting.
formatSockAddr :: SockAddr -> String
formatSockAddr (SockAddrInet _ host) =
  -- IPv4: Convert 32-bit host address to dotted-decimal notation (e.g., "192.168.1.1")
  intercalate "." (map show octets)
  where
    (a, b, c, d) = hostAddressToTuple host  -- Extract four 8-bit octets
    octets = [a, b, c, d]
formatSockAddr (SockAddrInet6 _ _ host _) =
  -- IPv6: Convert 128-bit host address to colon-hexadecimal notation (e.g., "2001:db8::1")
  intercalate ":" (map (printf "%x") segments)
  where
    -- Educational note: IPv6 addresses are 128 bits (eight 16-bit segments).
    -- The Haskell API gives us four 32-bit words, so we split each into two 16-bit segments.
    segments = concatMap split32 (tupleToList (hostAddress6ToTuple host))

    tupleToList (w1, w2, w3, w4) = [w1, w2, w3, w4]
    split32 :: Word32 -> [Word16]
    split32 w =
      -- Educational note: Bit shifting and masking extract the high and low 16 bits.
      -- shiftR n shifts right n bits, .&. is bitwise AND (masks to keep only lower bits).
      [ fromIntegral ((w `shiftR` 16) .&. 0xffff)  -- High 16 bits
      , fromIntegral (w .&. 0xffff)                -- Low 16 bits
      ]
formatSockAddr addr = show addr  -- Fallback for other address types
