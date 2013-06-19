{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
module SoOSiM.Components.ProcManager.Types where

import Control.Lens

import SoOSiM
import SoOSiM.Components.Common
import SoOSiM.Components.SoOSApplicationGraph

data PM_State
  = PM_State
  { _thread_graph :: ApplicationGraph
  , _rm           :: ComponentId
  , _pIO          :: ComponentId
  }

procMgrIState :: PM_State
procMgrIState = PM_State (ApplicationGraph "Generic" Nothing Nothing Nothing [] []) (-1) (-1)

makeLenses ''PM_State

data PM_Cmd
  = RunProgram String
  | TerminateProgram
  deriving (Typeable,Show)

data PM_Msg
  = PM_Void
  deriving (Typeable,Show)
