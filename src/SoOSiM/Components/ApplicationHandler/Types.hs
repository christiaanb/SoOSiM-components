{-# LANGUAGE DeriveDataTypeable #-}
module SoOSiM.Components.ApplicationHandler.Types where

import Data.HashMap.Strict (HashMap)

import SoOSiM
import SoOSiM.Components.SoOSApplicationGraph

data AH_State
  = AH_State (HashMap String ApplicationGraph)

data AH_Cmd
  = LoadProgram String
  | AddApps (HashMap String ApplicationGraph)
  deriving (Typeable,Show)

data AH_Msg
  = AH_AG ApplicationGraph
  | AH_Void
  deriving (Typeable,Show)
