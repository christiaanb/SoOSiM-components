module SoOSiM.Components.MemoryManager.Behaviour
  (memMgr)
where

import Control.Lens
import Control.Monad.State
import Data.List
import Data.Maybe
import SoOSiM
import SoOSiM.Types (ReturnAddress)

import SoOSiM.Components.Common
import SoOSiM.Components.MemoryManager.Interface
import SoOSiM.Components.MemoryManager.Types

memMgr ::
  MM_State
  -> Input MM_Cmd
  -> Sim MM_State
memMgr s i = execStateT (behaviour i) s >>= yield

behaviour (Message _ (Register base size) retAddr) = do
  directory <- use addressLookup
  case checkAddresss directory [(base,size)] of
    ([],_) -> do addressLookup %= insert (MemorySource base size Nothing)
                 parentM <- use parentMM
                 case parentM of
                   Nothing     -> return ()
                   Just parent -> do cId <- lift getComponentId
                                     lift $ inform parent (MemorySource base size (Just cId))
                 ackMM retAddr size

    (s,_)  -> error $ "Already registered: " ++ show s

behaviour (Message k (Read base size) retAddr) = do
  directory <- use addressLookup
  mmIDs <- case checkAddresss directory [(base,size)] of
             (m,[]) -> return m
             (m,l)  -> do lift $ traceMsg $ "Can't find: " ++ show (directory,m,l)
                          parentM <- use parentMM
                          case parentM of
                            Nothing     -> error $ "Address range(s) unregistered: " ++ show l
                            Just parent -> do (MM_Update m') <- lift $ request parent l
                                              addressLookup %= (++ m')
                                              return (m++m')

  lift $ traceMsg $ "Start reading: " ++ show mmIDs
  forM_ mmIDs $ (\(MemorySource b s idM) -> maybe (do lift $ compute 1 ()
                                                      lift $ traceMsg $ "Acknowledging read: " ++ show (returnAddress retAddr,b,s)
                                                      ackMM retAddr s
                                                  )
                                                  (\id_ -> do lift $ traceMsg $ "Forwarding read: " ++ show (returnAddress retAddr,b,s) ++ " to " ++ show id_
                                                              lift $ readOtherMem id_ retAddr b s)
                                                  idM)
  -- lift $ traceMsg $ "Finish reading: " ++ show mmIDs

behaviour (Message _ (Write base size) retAddr) = do
  directory <- use addressLookup
  mmIDs <- case checkAddresss directory [(base,size)] of
             (m,[]) -> return m
             (m,l)  -> do lift $ traceMsg $ "Can't find: " ++ show (directory,m,l)
                          parentM <- use parentMM
                          case parentM of
                            Nothing     -> error $ "Address range(s) unregistered: " ++ show l
                            Just parent -> do (MM_Update m') <- lift $ request parent l
                                              addressLookup %= (++ m')
                                              return (m++m')

  lift $ traceMsg $ "Start writing: " ++ show mmIDs
  forM_ mmIDs $ (\(MemorySource b s idM) -> maybe (do lift $ compute 1 ()
                                                      lift $ traceMsg $ "Acknowledging write: " ++ show (returnAddress retAddr,b,s)
                                                      ackMM retAddr s
                                                  )
                                                  (\id_ -> do lift $ traceMsg $ "Forwarding write: " ++ show (returnAddress retAddr,b,s) ++ " to " ++ show id_
                                                              lift $ writeOtherMem id_ retAddr b s)
                                                  idM)
  -- lift $ traceMsg $ "Finish writing: " ++ show mmIDs

behaviour (Message _ (UpdateP m) _) = do
  lift $ traceMsg $ "Received memory location update: " ++ show m
  addressLookup %= (m:)
  parentM <- use parentMM
  case parentM of
    Nothing     -> return ()
    Just parent -> lift $ inform parent m

behaviour (Message _ (Request l) retAddr) = do
  lift $ traceMsg $ "Receive memory location request: " ++ show l
  directory <- use addressLookup
  cId <- lift getComponentId
  let newM (MemorySource b s Nothing) = (MemorySource b s (Just cId))
      newM (MemorySource b s mId)     = (MemorySource b s mId)
  case checkAddresss directory l of
    (m,[]) -> updateChild retAddr (map newM m)
    (m,l') -> do parentM <- use parentMM
                 case parentM of
                   Nothing     -> error $ "Address range(s) unregistered: " ++ show l'
                   Just parent -> do (MM_Update m') <- lift $ request parent l'
                                     addressLookup %= (++ m')
                                     updateChild retAddr (map newM m ++ m')

behaviour _ = return ()

checkAddresss :: [MemorySource] -> [(Int,Int)] -> ([MemorySource],[(Int,Int)])
checkAddresss []       s  = ([],s )
checkAddresss _        [] = ([],[])
checkAddresss (ms:mss) wanted
  = let (foundS,leftOvers)   = (catMaybes >< concat) $ unzip $ map (checkAddress ms) wanted
        (foundS',leftOvers') = checkAddresss mss leftOvers
    in (foundS++foundS',leftOvers')

checkAddress (MemorySource baseS sizeS src) (base,size)
  = case overlapDiffRange (base,size) (baseS,sizeS) of
      Nothing        -> (Nothing,[(base,size)])
      Just ((b,r),l) -> (Just $ MemorySource b r src,l)

overlapDiffRange :: (Int,Int) -> (Int,Int) -> Maybe ((Int,Int),[(Int,Int)])
overlapDiffRange r1 r2 = fmap (endpointsToRange >< map endpointsToRange)
                       $ overlapDiff (rangeToEndpoints r1) (rangeToEndpoints r2)

rangeToEndpoints :: (Int,Int) -> (Int,Int)
rangeToEndpoints (base,size) = (base,base+size-1)

endpointsToRange :: (Int,Int) -> (Int,Int)
endpointsToRange (begin,end) = (begin,end-begin+1)

overlapDiff :: (Int,Int) -> (Int,Int) -> Maybe ((Int,Int),[(Int,Int)])
overlapDiff (pB,pE) (qB,qE)
  | dist > 0  = Nothing
  | otherwise = Just ((r1,r2),catMaybes [d1,d2])
  where
    r1   = max (min pB pE) (min qB qE)
    r2   = min (max pB pE) (max qB qE)
    d1   = if pB < qB then Just (pB,qB-1) else Nothing
    d2   = if pE > qE then Just (qE+1,pE) else Nothing
    dist = r1 - r2

inform :: ComponentId -> MemorySource -> Sim ()
inform pId m = do
  traceMsg $ "Notifying parent on memory registration: " ++ show (pId,m)
  notify MemoryManager pId (UpdateP m)

request :: ComponentId -> [(Int,Int)] -> Sim MM_Msg
request pId l = do
  traceMsg $ "Asking parent(" ++ show pId ++ ") for memory ranges: " ++ show l
  resp <- invoke MemoryManager pId (Request l)
  traceMsg $ "Response from parent(" ++ show pId ++ "): " ++ show resp
  return resp

writeOtherMem :: ComponentId -> ReturnAddress -> Int -> Int -> Sim ()
writeOtherMem cId retAddr base size = do
  curId <- getComponentId
  invokeAsync MemoryManager cId (Write base size) (mmAsyncHandler curId retAddr)

readOtherMem :: ComponentId -> ReturnAddress -> Int -> Int -> Sim ()
readOtherMem cId retAddr base size = do
  curId <- getComponentId
  invokeAsync MemoryManager cId (Read base size) (mmAsyncHandler curId retAddr)

mmAsyncHandler :: ComponentId -> ReturnAddress -> MM_Msg -> Sim ()
mmAsyncHandler cId retAddr msg = respondS MemoryManager (Just cId) retAddr msg

ackMM r s       = lift $ respond MemoryManager r (MM_ACK s)
updateChild r m = lift $ respond MemoryManager r (MM_Update m)
