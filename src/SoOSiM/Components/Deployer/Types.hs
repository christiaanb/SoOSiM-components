{-# LANGUAGE DeriveDataTypeable #-}
module SoOSiM.Components.Deployer.Types where

import Control.Concurrent.STM.TVar (TVar)

import SoOSiM
import SoOSiM.Components.Common
import SoOSiM.Components.Thread

data DM_Cmd
  = StartThread ComponentId     -- ^ ComponentID of the scheduler controlling the thread
                [ ( ThreadId    -- ^ (copy of) ThreadId
                  , TVar Thread -- ^ Reference to the thread meta-data
                  , NodeId      -- ^ Node on which to instantiate the thread
                  , String      -- ^ Name of the application the thread belongs to
                  , (Int,Int)   -- ^ Memory to reserve
                  )
                ]
  deriving (Typeable)

instance Show DM_Cmd where
  show _ = "StartThread"

data DM_Msg
  = DM_TH [ComponentId]
  deriving (Typeable,Show)
