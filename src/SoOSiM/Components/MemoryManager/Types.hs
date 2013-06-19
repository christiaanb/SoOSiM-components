{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
module SoOSiM.Components.MemoryManager.Types where

import Control.Lens

import SoOSiM
import SoOSiM.Components.Common
import SoOSiM.Components.SoOSApplicationGraph

data MemorySource
  = MemorySource
  { _baseAddress :: Int
  , _scope       :: Int
  , _sourceId    :: Maybe ComponentId
  } deriving (Typeable,Show,Ord,Eq)

makeLenses ''MemorySource

data MM_State
  = MM_State
  { _parentMM      :: Maybe ComponentId
  , _addressLookup :: [MemorySource]
  }

memMgrIState :: MM_State
memMgrIState = MM_State Nothing []

makeLenses ''MM_State

data MM_Cmd
  = Register Int Int
  | Read     Int Int
  | Write    Int Int
  | Request  [(Int,Int)]
  | UpdateP  MemorySource
  deriving (Typeable,Show)

data MM_Msg
  = MM_Void
  | MM_ACK Int
  | MM_Update [MemorySource]
  deriving (Typeable,Show)
