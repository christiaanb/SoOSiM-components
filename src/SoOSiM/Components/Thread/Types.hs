{-# LANGUAGE TemplateHaskell #-}
module SoOSiM.Components.Thread.Types where

import Control.Lens
import Control.Concurrent.STM.TQueue

import SoOSiM.Components.Common
import SoOSiM.Components.ResourceDescriptor
import SoOSiM.Components.SoOSApplicationGraph

data ThreadState = Blocked | Waiting | Executing | Killed
  deriving Eq

data Deadline = Infinity | Exact Int
  deriving (Eq,Show)

instance Ord Deadline where
  compare Infinity Infinity   = EQ
  compare (Exact i) Infinity  = LT
  compare Infinity (Exact i)  = GT
  compare (Exact i) (Exact j) = compare i j

data Thread
  = Thread
  { -- | The thread unique id
    _threadId        :: ThreadId
    -- | number of incoming \"ports\", each in-port has an id from 0 to (n_in - 1)
  , _n_in            :: Int
    -- | number of outgoing \"ports\", each out-port has an id form 0 to (n_in - 1)
  , _n_out           :: Int
    -- | incoming ports: ntokens per port
  , _in_ports        :: [TQueue (Int,Int)]
    -- | outgoing links
    --
    -- contains the pair (thread_dest_id, in_port_id) of the destination threads
  , _out_ports       :: [(ThreadId,TQueue (Int,Int))]
    -- | Number of (simulation) cycles needed to complete one instance of the thread
  , _exec_cycles     :: Int
    -- | resource requirements
  , _rr              :: ResourceDescriptor
    -- | an enumerations: BLOCKED; READY; EXECUTING
  , _execution_state :: ThreadState
    -- | The id of the resource where this thread is executing
  , _res_id          :: ResourceId
    -- | It is the SimTime when the current instance has been activated
    -- this is update by the Scheduler.wake_up_threads when the thres is
    -- moved from blocked to ready.
    -- it can be needed to sort ready thread in FIFO order
  , _activation_time :: Int
  , _program         :: [AppCommand]
  , _localMem        :: (Int,Int)
  , _relativeDeadlineOut :: Deadline
  , _relativeDeadlineIn  :: Deadline
  }

instance Show Thread where
  show = show . _threadId

makeLenses ''Thread
