-------------------------------------------------------------------------------
-- |
-- Module      :  Profile.Live.Server.State
-- Copyright   :  (c) Anton Gushcha 2016
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  ncrashed@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Utilities to watch after eventlog state: alive threads, existing caps and
-- tasks. When new client is connected, we resend relevant events to the remote
-- side. Having relevant state simplifies client work of correct visualization. 
--
------------------------------------------------------------------------------
module Profile.Live.Server.State(
    EventlogState
  , newEventlogState
  , updateEventlogState
  ) where 

import Control.DeepSeq 
import Data.Binary.Serialise.CBOR 
import GHC.Generics 
import GHC.RTS.Events 
import Profile.Live.Server.State.Thread 

-- | Storage of all state of eventlog protocol.
--
-- It contains info about threads, caps, tasks, whether
-- GC is performed right now.
data EventlogState = EventlogState {
    eventlogThreads :: !ThreadsState 
    -- | If 'Just' the GC is performed and the value contains time of GC begin
  , eventlogGC :: !(Maybe Timestamp)
  } deriving (Generic)

instance NFData EventlogState
instance Serialise EventlogState

-- | Initiate new eventlog state
newEventlogState :: EventlogState 
newEventlogState = EventlogState {
    eventlogThreads = newThreadsState
  , eventlogGC = Nothing
  }

-- | Update state with next event
updateEventlogState :: Event -> EventlogState -> EventlogState
updateEventlogState !e !es 
  | isThreadEvent e = es { eventlogThreads = updateThreadsState e (eventlogThreads es) }
  | isGCEvent e = es { eventlogGC = updateGCState e (eventlogGC es) }
  | otherwise = es 

-- | Update current GC state
updateGCState :: Event -> Maybe Timestamp -> Maybe Timestamp
updateGCState Event{..} mt = case evSpec of 
  StartGC -> Just evTime 
  GCWork -> maybe (Just evTime) Just mt
  GCIdle -> maybe (Just evTime) Just mt
  GlobalSyncGC -> maybe (Just evTime) Just mt
  GCDone -> Nothing
  EndGC -> Nothing
  _ -> mt 

-- | Returns 'True' if the event is related to GC
isGCEvent :: Event -> Bool 
isGCEvent e = case evSpec e of 
  RequestSeqGC {} -> True
  RequestParGC {} -> True
  StartGC {} -> True
  GCWork {} -> True
  GCIdle {} -> True
  GCDone {} -> True
  EndGC {} -> True
  GlobalSyncGC {} -> True
  GCStatsGHC {} -> True