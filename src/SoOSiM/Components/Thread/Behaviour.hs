{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
module SoOSiM.Components.Thread.Behaviour where

import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Control.Lens
import Control.Monad
import Data.Maybe

import SoOSiM
import SoOSiM.Components.Common

import SoOSiM.Components.Thread.Interface
import SoOSiM.Components.Thread.Types

data TH_State
  = TH_State
  { _actual_id       :: ThreadId
  , _sched_id        :: ComponentId
  , _thread_state    :: Maybe (TVar Thread)
  , _appName         :: String
  }

makeLenses ''TH_State

data TH_Cmd
  = TH_Start
  | TH_Stop
  deriving Typeable

data TH_Msg
  = TH_Void
  deriving Typeable

threadIState :: TH_State
threadIState = TH_State (-1) (-1) Nothing ""

threadBehaviour ::
  TH_State
  -> Input TH_Cmd
  -> Sim TH_State
threadBehaviour s@(TH_State _ _ Nothing _) _ = yield s

threadBehaviour s (Message _ TH_Start _) = do
  let ts = fromJust $ s ^. thread_state
  t <- runSTM $ readTVar ts
  case (t ^. execution_state) of
    Executing -> do
      -- Read timestamps from inputs ports
      timestamps <- runSTM $ mapM readTQueue (t ^. in_ports)

      -- Execute computation
      compute ((t ^. exec_cycles) - 1) ()
      traceMsgTag "Finished" ("ThreadEnd " ++ (s ^. appName) ++ ".T" ++ show (t ^. threadId) ++ " Proc" ++ show (t ^. res_id))

      -- Write to output ports
      let newTime = minimum timestamps
      runSTM $ mapM_ (\(_,q) -> writeTQueue q newTime) (t^.out_ports)

      -- Signal scheduler that thread has completed
      runSTM $ modifyTVar' ts (execution_state .~ Waiting)

      yield s

    -- Waiting to start
    Waiting -> do
      traceMsg "Waiting"
      yield s

    -- Finished one execution cycle
    Blocked -> do
      traceMsg "Stopping"
      yield s

    Killed -> do
      traceMsg "Killed"
      yield s

threadBehaviour s (Message _ TH_Stop _) = stop

threadBehaviour s _ = yield s
