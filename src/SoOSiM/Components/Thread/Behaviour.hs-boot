module SoOSiM.Components.Thread.Behaviour
  (TH_State(..),TH_Cmd(..),TH_Msg,threadBehaviour,threadIState)
where

import Control.Concurrent.STM.TVar (TVar)

import SoOSiM
import SoOSiM.Components.Common
import SoOSiM.Components.Thread.Types

data TH_State
  = TH_State
  { _actual_id    :: ThreadId
  , _sched_id     :: ComponentId
  , _thread_state :: Maybe (TVar Thread)
  , _appName      :: String
  }


data TH_Cmd = TH_Start | TH_Stop
data TH_Msg

instance Typeable TH_Cmd
instance Show     TH_Cmd
instance Typeable TH_Msg

threadBehaviour ::
  TH_State
  -> Input TH_Cmd
  -> Sim TH_State

threadIState :: TH_State
