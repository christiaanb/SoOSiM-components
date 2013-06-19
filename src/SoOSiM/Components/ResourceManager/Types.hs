{-# LANGUAGE DeriveDataTypeable #-}
module SoOSiM.Components.ResourceManager.Types where

import Data.HashMap.Strict (HashMap)

import SoOSiM
import SoOSiM.Components.ResourceDescriptor
import SoOSiM.Components.Common

type ResourceRequestList = [(ResourceDescriptor,Int)]
type ResourceList        = HashMap ResourceId ResourceDescriptor
type ResourceInverseList = HashMap ResourceDescriptor [ResourceId]
type ResourceFreeList    = [ResourceId]
type ResourceBusyList    = [(ResourceId,AppId)]

data RM_State
  = RM_State
  { resources      :: ResourceList
  , resources_inv  :: ResourceInverseList
  , free_resources :: ResourceFreeList
  , busy_resources :: ResourceBusyList
  , dist_method    :: String
  }

data RM_Cmd
  = AddResource ResourceId ResourceDescriptor
  | RequestResources AppId ResourceRequestList
  | FreeAllResources AppId
  | FreeResources AppId [ResourceId]
  | GetResourceDescription ResourceId
  deriving (Typeable,Show)

data RM_Msg
  = RM_Resources [ResourceId]
  | RM_Descriptor (Maybe ResourceDescriptor)
  | RM_Void
  deriving (Typeable,Show)
