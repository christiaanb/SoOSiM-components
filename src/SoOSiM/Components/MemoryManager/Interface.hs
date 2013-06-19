{-# LANGUAGE TypeFamilies #-}
module SoOSiM.Components.MemoryManager.Interface where

import SoOSiM
import {-# SOURCE #-} SoOSiM.Components.MemoryManager.Behaviour (memMgr)
import SoOSiM.Components.MemoryManager.Types

data MemoryManager = MemoryManager

instance ComponentInterface MemoryManager where
  type State MemoryManager   = MM_State
  type Receive MemoryManager = MM_Cmd
  type Send MemoryManager    = MM_Msg
  initState                  = const memMgrIState
  componentName              = const "Memory Manager"
  componentBehaviour         = const memMgr

createMemoryManager :: (Maybe NodeId) -> Maybe ComponentId -> Sim ComponentId
createMemoryManager n p = componentLookupN n MemoryManager >>= \x -> case x of
    Nothing  -> createComponentNPS n Nothing (Just iState) MemoryManager
    Just cId -> return cId
  where
    iState = memMgrIState { _parentMM = p }

registerMem :: (Maybe ComponentId) -> Int -> Int -> Sim ()
registerMem (Just cId) base size = invoke MemoryManager cId (Register base size) >> return ()
registerMem Nothing base size = componentLookup MemoryManager >>= \x -> case x of
    Nothing  -> error "no instantiated memory manager"
    Just cId -> invoke MemoryManager cId (Register base size) >> return ()

readMem :: Int -> Int -> Sim ()
readMem base size = componentLookup MemoryManager >>= \x -> case x of
    Nothing  -> error "no instantiated memory manager"
    Just cId -> do (MM_ACK s) <- invoke MemoryManager cId (Read base size)
                   runUntilAll cId size [s]

writeMem :: Int -> Int -> Sim ()
writeMem base size = componentLookup MemoryManager >>= \x -> case x of
    Nothing  -> error "no instantiated memory manager"
    Just cId -> do (MM_ACK s) <- invoke MemoryManager cId (Write base size)
                   runUntilAll cId size [s]

runUntilAll :: ComponentId -> Int -> [Int] -> Sim ()
runUntilAll cId s ss = case compare s (sum ss) of
  LT -> do (MM_ACK s') <- expect MemoryManager cId
           runUntilAll cId s (s':ss)
  _  -> return ()
