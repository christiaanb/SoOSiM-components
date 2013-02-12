{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
module SoOSiM.Components.Scheduler.Types where

import Control.Concurrent.STM.TVar (TVar)
import Control.Lens                (makeLenses)
import Data.HashMap.Strict         (HashMap,empty)

import SoOSiM
import SoOSiM.Components.Common
import SoOSiM.Components.ResourceDescriptor
import SoOSiM.Components.Thread

data ResStatus = IDLE_RES | BUSY_RES
  deriving (Eq,Show)

data SC_State
  = SC_State
  { -- | ComponentId of the processManager
    _pm           :: ComponentId
    -- | The list of threads managed by this scheduler
  , _thread_list  :: HashMap ThreadId (TVar Thread)
    -- | The list of read threads ids
  , _ready        :: [ThreadId]
    -- | The list of blocked threads ids
  , _blocked      :: [ThreadId]
    -- | The list of executing threads
    --
    -- This map tells us where each thread is executing
  , _exec_threads :: HashMap ThreadId ResourceId
    -- | This maps records the resource status for each resource
  , _res_map      :: HashMap ResourceId ResStatus
    -- | This map associates resource id with resource descriptor
    -- this is sort of not very useful, because this relationship is already inside the
    -- Resource datatype
  , _res_types    :: HashMap ResourceId ResourceDescriptor
    -- | This associate each thread with the set of resources on which it can be executed
    -- this map is prepared by the Process Manager
  , _thread_res_allocation :: HashMap ThreadId [ResourceId]
    -- | This associates each threadId to a componentId
  , _components   :: HashMap ThreadId ComponentId
    -- | Method by which to sort the ready queue
  , _sortingMethod :: Thread -> Thread -> Ordering
  , _appName :: String
  }

-- Sort ready list by FIFO (i.e. arrival time)
byArrivalTime :: Thread -> Thread -> Ordering
byArrivalTime t1 t2 = compare (_activation_time t1) (_activation_time t2)

schedIState :: SC_State
schedIState = SC_State (-1) empty [] [] empty empty empty empty empty byArrivalTime ""

makeLenses ''SC_State

data SC_Cmd
  = Init (HashMap ThreadId (TVar Thread))
         [(ResourceId,ResourceDescriptor)]
         (HashMap ThreadId [ResourceId])
         (Maybe String)
         String
  | ThreadCompleted ThreadId
  | WakeUpThreads
  | FindFreeResources ThreadId
  | StopSched
  deriving Typeable

data SC_Msg
  = SC_Void
  | SC_Node (Maybe ResourceId)
  deriving Typeable
