{-# LANGUAGE TypeFamilies #-}
module SoOSiM.Components.ResourceManager.Interface where

import Data.HashMap.Strict (empty)

import SoOSiM
import SoOSiM.Components.Common
import SoOSiM.Components.ResourceDescriptor

import {-# SOURCE #-} SoOSiM.Components.ResourceManager.Behaviour (behaviour)
import SoOSiM.Components.ResourceManager.Types

data ResourceManager = ResourceManager

instance ComponentInterface ResourceManager where
  type State ResourceManager   = RM_State
  type Receive ResourceManager = RM_Cmd
  type Send ResourceManager    = RM_Msg
  initState                    = const (RM_State empty empty [] [] "all")
  componentName                = const "Resource Manager"
  componentBehaviour           = const behaviour

resourceManager :: String -> Sim ComponentId
resourceManager dist = do
    componentLookup ResourceManager >>= \x -> case x of
      Nothing  -> createComponentNPS Nothing Nothing (Just iState) ResourceManager
      Just cId -> return cId
  where
    iState = RM_State empty empty [] [] dist

addResource :: ComponentId -> ResourceId -> ResourceDescriptor -> Sim ()
addResource cId rId rd = notify ResourceManager cId (AddResource rId rd)

requestResources :: ComponentId -> AppId -> ResourceRequestList -> Sim [ResourceId]
requestResources cId appId rl = invoke ResourceManager cId (RequestResources appId rl) >>= (\(RM_Resources r) -> return r)

freeAllResources :: ComponentId -> AppId -> Sim ()
freeAllResources cId appId = notify ResourceManager cId (FreeAllResources appId)

freeResources :: ComponentId -> AppId -> [ResourceId] -> Sim ()
freeResources cId appId rIds = notify ResourceManager cId (FreeResources appId rIds)

getResourceDescription :: ComponentId -> ResourceId -> Sim (Maybe ResourceDescriptor)
getResourceDescription cId rId = invoke ResourceManager cId (GetResourceDescription rId) >>= (\(RM_Descriptor r) -> return r)
