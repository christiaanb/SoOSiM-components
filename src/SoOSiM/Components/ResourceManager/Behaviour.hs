{-# LANGUAGE TupleSections #-}
module SoOSiM.Components.ResourceManager.Behaviour where

import           Control.Arrow       (first,second)
import           Data.Char           (toLower)
import qualified Data.HashMap.Strict as HashMap
import           Data.List           (mapAccumL,intersect,(\\),partition)

import SoOSiM
import SoOSiM.Components.ResourceDescriptor

import SoOSiM.Components.ResourceManager.Interface
import SoOSiM.Components.ResourceManager.Types

behaviour ::
  RM_State
  -> Input RM_Cmd
  -> Sim RM_State
behaviour s (Message _ (AddResource rId rd) retAddr) = do
  let rs  = HashMap.insert rId rd (resources s)
      rsI = HashMap.insertWith (flip (++)) rd [rId] (resources_inv s)
      s'  = s { resources = rs, resources_inv = rsI, free_resources = (free_resources s) ++ [rId] }
  yield s'

behaviour s (Message _ (RequestResources appId rsList) retAddr) = do
  let (free',ids) = assignFree s rsList
      busy        = map (,appId) ids
      s'          = s { free_resources = free', busy_resources = (busy_resources s) ++ busy }
  traceMsg ("REQ: " ++ show (rsList,ids))
  respond ResourceManager retAddr (RM_Resources ids)
  yield s'

behaviour s (Message _ (FreeResources appId) retAddr) = do
  let (freed,busy') = first (map fst) $ partition ((== appId) . snd) (busy_resources s)
      s'            = s { free_resources = (free_resources s) ++ freed, busy_resources = busy' }
  yield s'

behaviour s (Message _ (GetResourceDescription rId) retAddr) = do
  let rdM = HashMap.lookup rId (resources s)
  respond ResourceManager retAddr (RM_Descriptor rdM)
  yield s

behaviour s _ = yield s

checkFree :: String -> ResourceFreeList -> ([ResourceId],Int) -> (ResourceFreeList,[ResourceId])
checkFree dm free (keys,needed)
  = let keys'  = intersect keys free
        keys'' = take needed $ case dm of
                     "all"  -> keys'
                     "half" -> take (ceiling $ (fromIntegral $ length keys') / 2) keys'
                     _      -> keys'
        free'  = free \\ keys''
    in  (free',keys'')

assignFree :: RM_State -> ResourceRequestList -> (ResourceFreeList,[ResourceId])
assignFree s rsList = (free',givenIds)
  where
    available        = map (\(rTy,_) -> concat $
                                        HashMap.elems $
                                        HashMap.filterWithKey (\k _ -> isComplient k rTy) (resources_inv s)
                           ) rsList
    wanted           = zip available (map snd rsList)
    dm               = map toLower (dist_method s)
    (free',givenIds) = second concat $ mapAccumL (checkFree dm) (free_resources s) wanted
