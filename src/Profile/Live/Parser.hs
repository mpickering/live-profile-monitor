module Profile.Live.Parser(
    redirectEventlog
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMChan
import Control.Exception (bracket)
import Control.Monad
import Data.IORef
import Data.Monoid
import Foreign hiding (void)
import GHC.RTS.Events hiding (ThreadId)
import GHC.RTS.Events.Incremental
import System.Log.FastLogger

import qualified Data.ByteString as B

import Profile.Live.Options
import Profile.Live.Pipe
import Profile.Live.Protocol.State
import Profile.Live.Protocol.Utils
import Profile.Live.State
import Profile.Live.Termination

-- | Initialise link with C world that pipes data from FIFO file (or named pipe on Windows)
initMemoryPipe :: FilePath -- ^ Pipe name
  -> Word64 -- ^ Chunk size
  -> IO (TChan B.ByteString, IO ())
initMemoryPipe pipeName buffSize = do
  chan <- newTChanIO
  stopPipe <- startPipe PipeOptions {
      pipeName = pipeName
    , pipeBufferSize = buffSize
    , pipeCallback = atomically . writeTChan chan
    }
  return (chan, stopPipe)

-- | Creates thread that pipes eventlog from memory into incremental parser
redirectEventlog :: LoggerSet -- ^ Monitor logger
  -> LiveProfileOpts -- ^ Options of the monitor
  -> TerminationPair -- ^ Termination protocol
  -> EventTypeChan -- ^ Channel for event types, closed as soon as first event occured
  -> EventChan -- ^ Channel for events
  -> IO (ThreadId, IORef EventlogState) -- ^ Forks new thread with incremental parser
redirectEventlog logger LiveProfileOpts{..} term eventTypeChan eventChan = do
  stateRef <- newIORef $ newEventlogState
  tid <- forkIO $ do
    labelCurrentThread "Parser"
    logProf logger "Parser thread started"
    bracket (initMemoryPipe eventLogPipeName eventLogChunkSize) snd $ \(pipe, _) -> do
      untilTerminatedPair term $ go stateRef pipe
        (withHeader $ \h@Header{..} leftover -> do
          Produce (atomically $ mapM_ putEventType' eventTypes)
           ((handle stateRef <$> (decodeEvents h)) `pushChunk` leftover))
    logProf logger "Parser thread terminated"
  return (tid, stateRef)
  where
  handle :: IORef EventlogState -> Event -> IO ()
  handle stateRef e = do
    atomicModifyIORef' stateRef $ \state -> (updateEventlogState e state, ())
    atomically $ putEvent' e

  go :: IORef EventlogState -> TChan B.ByteString -> Decoder (IO ()) -> IO ()
  go !stateRef !pipe !parserState = do
    case parserState of
      Produce e p -> do
        -- Update eventlog state

{-
        -- If the first item, we should pass header into channel
        mhmsg <- atomically $ do
          closed <- isClosedTBMChan eventTypeChan
          if not closed then do
              msgs <- putHeader' parserState''
              closeTBMChan eventTypeChan
              return msgs
            else return Nothing
        whenJust mhmsg $ logProf logger
        -}

        -- Passing event to event channel (to the server)
        --logProf logger $ "Putting event to channel: " <> showl e
        e
        go stateRef pipe p
      Consume k -> do
        datum <- atomically $ readTChan pipe
        go stateRef pipe (k datum)
      Done _leftover -> logProf logger "done"
      Error _lo er -> logProf logger $ "parserThread error: " <> toLogStr er

  putEventType' = putChannel eventTypeChan
  putEvent' = putChannel eventChan

  -- If channel is full, clear it and start again
  putChannel :: forall a . TBMChan a -> a -> STM ()
  putChannel chan i = do
    full <- isFullTBMChan chan
    when full $ clearTBMChan chan
    writeTBMChan chan i

  clearTBMChan :: forall a . TBMChan a -> STM ()
  clearTBMChan chan = do
    mval <- tryPeekTBMChan chan
    case mval of
      Just (Just _) -> clearTBMChan chan
      _ -> return ()
