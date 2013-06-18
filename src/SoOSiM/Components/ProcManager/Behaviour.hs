{-# LANGUAGE TupleSections #-}
module SoOSiM.Components.ProcManager.Behaviour
  (procMgr)
where

import Control.Arrow                 (second)
import Control.Applicative
import Control.Concurrent.STM.TVar   (newTVar)
import Control.Concurrent.STM.TQueue (newTQueue,writeTQueue)
import Control.Lens
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Writer
import           Data.BinPack
import           Data.Char           (toLower)
import qualified Data.Foldable       as F
import           Data.Function       (on)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List           as L
import qualified Data.Map            as Map
import Data.Maybe (catMaybes,isJust,fromJust,mapMaybe,fromMaybe)
import           Data.Ord
import qualified Data.Traversable as T

import qualified SoOSiM
import SoOSiM hiding (traceMsg)
import SoOSiM.Components.ApplicationHandler
import SoOSiM.Components.Common
import SoOSiM.Components.Deployer
import SoOSiM.Components.MemoryManager
import SoOSiM.Components.PeriodicIO
import SoOSiM.Components.ResourceDescriptor
import SoOSiM.Components.ResourceManager
import SoOSiM.Components.ResourceManager.Types
import SoOSiM.Components.SoOSApplicationGraph
import SoOSiM.Components.Scheduler
import SoOSiM.Components.Thread

import SoOSiM.Components.ProcManager.Interface
import SoOSiM.Components.ProcManager.Types

procMgr ::
  PM_State
  -> Input PM_Cmd
  -> Sim PM_State
procMgr s i = execStateT (behaviour i) s >>= yield

type ProcMgrM a = StateT PM_State Sim a

behaviour ::
  Input PM_Cmd
  -> ProcMgrM ()
behaviour (Message _ (RunProgram fN) retAddr) = do
  lift $ traceMsgTag ("Begin application " ++ fN) ("AppBegin " ++ fN)
  -- invokes the Application Handler
  thread_graph <- lift $ applicationHandler >>= flip loadProgram fN

  -- Create all threads
  let deadlines = map (inferDeadline (edges thread_graph)) (vertices thread_graph)
  let threads = HashMap.fromList
              $ zipWith (\v (dO,dI) -> ( v_id v
                                       , newThread (v_id v)
                                                   (executionTime v)
                                                   (appCommands v)
                                                   (memRange v)
                                                   dO
                                                   dI
                                       ))
                (vertices thread_graph) deadlines

  (th_all,rc) <- untilJust $ do
    -- Now we have to contact the Resource Manager

    -- prepares a list of resource requirements,
    -- for each resource description this list contains
    -- the number of desired resources
    let rl = case (recSpec thread_graph) of
               Just rl' -> rl'
               Nothing  -> prepareResourceRequestListSimple thread_graph

    -- the resource manager for this Process manager should be
    -- uniquely identified. here I am assuming that we have a
    -- singleton implementation of the resource manager for this file:
    -- function instance() will locate the resource manager for this
    -- instance of the process manager.
    rId  <- use rm
    pmId <- lift $ getComponentId
    res  <- lift $ requestResources rId pmId rl

    -- Now, if necessary I should allocate threads to resources. in
    -- this first sample implementation, I ignore the content of the
    -- resource descriptors, and I assume that all thread and
    -- resources have the correct ISA.

    -- create the list of resources
    rc <- mapM (\x ->
                  do d <- use rm >>= (\rId -> lift $ getResourceDescription rId x)
                     return (x,maybe (error "fromJust: resources") id d)
               ) res

    -- Allocation algorithms. Here I just statically allocate
    -- threads to resources in the simplest possible way
    -- let th_allM = allocate threads rc assignResourceSimple ()
    let th_allM = case (fmap (map toLower) $ allocSort thread_graph) of
                    Just "minwcet" -> allocate threads rc assignResourceMinWCET 0
                    Just "bestfit" -> allocate threads rc assignResourceBestFit ()
                    _              -> allocate threads rc assignResourceSimple ()

    -- Another alternative allocation strategy
    -- let th_allM = allocate_global threads' res
    return $ fmap (,rc) th_allM

  -- Make connections
  tbqueues <- lift $ runSTM $ replicateM (length $ edges thread_graph) newTQueue
  startTime <- lift $ getTime
  ((threads',[]), (periodicEdges,deadlineEdges)) <- runWriterT $ F.foldrM
         (\e (t,(q:qs)) -> do
                -- Create the in_port of the destination thread, and
                -- initialize it with the number of tokens
            let t'  = if (end e < 0)
                        then t
                        else HashMap.adjust (in_ports %~ (++ [q])) (end e) t

                -- create the out_ports of the source thread, and
                -- initialize it with the pair (thread_id, destination port)
                t'' = if (start e < 0)
                        then t'
                        else HashMap.adjust (out_ports %~ (++ [(end e,q)])) (start e) t'

            -- Instantiate periodic edges
            case (periodic e) of
              Nothing -> lift $ lift $ runSTM $ replicateM_ (n_tokens e) (writeTQueue q (startTime,startTime))
              Just p  -> case n_tokens e of
                            0 -> return ()
                            n -> do
                              lift $ lift $ runSTM $ writeTQueue q (startTime,startTime)
                              tell ([(q,0,p,n-1)],[])

            -- Instantiate deadline edges
            case (deadline e) of
              Nothing -> return ()
              Just n  -> tell ([],[(q,n,fN,start e,(-1))])

            return (t'',qs)
         )
         (threads,tbqueues)
       $ edges thread_graph

  traceMsg $ "ThreadAssignment(" ++ fromMaybe "SIMPLE" (allocSort thread_graph) ++ "): "  ++ show th_all
  periodicEdgesS <- lift $ runSTM $ newTVar periodicEdges

  -- Now initialize the scheduler, passing the list of
  -- threads, and the list of resources
  threadVars <- T.mapM (lift . runSTM . newTVar) threads'
  pmId <- lift $ getComponentId
  sId  <- lift $ newScheduler pmId

  -- Deploy all the threads
  dmId <- lift $ deployer
  let thInfo = map (\tId -> ( tId
                            , threadVars HashMap.! tId
                            , head $ th_all HashMap.! tId
                            , fN
                            , (threads HashMap.! tId) ^. localMem
                            ))
                   (HashMap.keys th_all)
  thCIDs <- lift $ deployThreads dmId sId thInfo
  let cmMap = HashMap.fromList $ zip (HashMap.keys th_all) thCIDs

  traceMsg $ "Starting scheduler"
  lift $ initScheduler sId threadVars rc th_all (schedulerSort thread_graph) fN periodicEdgesS cmMap

  -- Initialize Periodic I/O if needed
  unless (null periodicEdges && null deadlineEdges) $ do
    let pIOState = PeriodicIOS (Just periodicEdgesS,deadlineEdges,sId)
    newId <- lift $ SoOSiM.createComponentNPS Nothing Nothing (Just pIOState) (PeriodicIO fN)
    pIO .= newId


behaviour (Message _ TerminateProgram retAddr) = do
  fN <- fmap appName $ use thread_graph

  -- The program has completed, free the resources
  pmId <- lift $ getComponentId
  rId  <- use rm
  res  <- lift $ freeAllResources rId pmId

  -- Stop the scheduler
  lift $ stopScheduler (returnAddress retAddr)

  -- Stop the periodic IO
  pIOid <- use pIO
  unless (pIOid < 0) (lift $ stopPIO pIOid fN)

  -- Stop the process manager
  lift stop

behaviour _ = return ()

prepareResourceRequestListSimple ::
  ApplicationGraph
  -> ResourceRequestList
prepareResourceRequestListSimple ag = rl
  where
    -- anyRes is a constant that means "give me any resource that you have"

    -- this will ask for a number of processors equal to the number of threads
    rl = [(ANY_RES,numberOfVertices ag)]

allocate ::
  HashMap ThreadId Thread
  -> [(ResourceId,ResourceDescriptor)]
  -> AssignProc a
  -> a
  -> Maybe (HashMap ThreadId [ResourceId])
allocate threads resMap assignResource initR = thAll
  where
    -- Build the inverse of resMap
    resMapI = Map.toList $ foldl
                (\m (rId,r) ->
                  Map.alter
                    (\x -> case x of
                      Nothing -> Just [(rId,initR)]
                      Just rs -> Just ((rId,initR):rs)
                    ) r m
                ) Map.empty resMap

    -- Distribute threads over compatible resources
    (thsLeft,resThreadMap) =
      T.mapAccumL (\ths (rd,rIds) ->
                     let (thsComp,thsNotcomp) = partitionHashMap (\t -> (t^.rr) `isComplient` rd) ths
                         thsComp'             = HashMap.elems thsComp
                     in (thsNotcomp,if HashMap.null thsComp then Nothing else Just (thsComp',rIds))
                  ) threads
                    resMapI

    -- Load-balance resource assignment
    thAll = case HashMap.null thsLeft of
              False -> Nothing
              True  -> Just $ L.foldl' (HashMap.unionWith (++)) HashMap.empty $ map assignResource (catMaybes resThreadMap)

type AssignProc a =
  ([Thread],[(ResourceId,a)])
  -> HashMap ThreadId [ResourceId]

-- | Assign the resource and rotate the resourceId list to balance
-- the assignment of threads to resources
assignResourceSimple :: AssignProc ()
assignResourceSimple (ths,rds) = HashMap.fromList $ snd $ T.mapAccumL
    (\(rId:rIds) t -> (rIds ++ [rId], (t^.threadId,[fst rId]))
    ) rds ths'
  where
    ths' = reverse $ L.sortBy (comparing (^.exec_cycles)) ths

-- | Assign the resource and insert the assigned resource a list
-- ordered according to the accumulate exec_cycles
assignResourceMinWCET :: AssignProc Int
assignResourceMinWCET (ths,rds) = HashMap.fromList $ snd $ T.mapAccumL
    (\(rId:rIds) t -> let rId'  = second (+(t^.exec_cycles)) rId
                      in (L.insertBy (comparing snd) rId' rIds,(t^.threadId,[fst rId]))
    ) rds ths'
  where
    ths' = reverse $ L.sortBy (comparing (^.exec_cycles)) ths

assignResourceBestFit :: AssignProc ()
assignResourceBestFit (ths,rds) = case null left of
                                    True  -> HashMap.fromList $ concat rdMap
                                    False -> error $ "Can't assign threads (tId,utility): " ++ show (map (\t -> (t,threadUtility t)) left) ++ "\n: Bin Content: " ++ show bins
  where
    (bins,left) = binpack BestFit Decreasing threadUtility (emptyBins 1.0 (length rds)) ths
    rdMap       = zipWith (\(_,ths') rId -> map (\t -> (t^.threadId,[fst rId])) ths') bins rds

threadUtility :: Thread -> Float
threadUtility t = case (t ^. relativeDeadlineIn, t ^. relativeDeadlineOut) of
  (Infinity, Exact dOut) -> (fromIntegral $ t ^. exec_cycles) / (fromIntegral dOut)
  (Exact dIn, Exact dOut) -> let dlDiff = dOut - dIn
                             in if (dlDiff < 1)
                                 then error $ "Thread with ID: " ++ show (t ^. threadId) ++ " has invalid deadlines (inbound,outbound): " ++ show (dIn,dOut)
                                 else (fromIntegral $ t ^. exec_cycles) / (fromIntegral dlDiff)
  (dIn,dOut) -> error $ "Thread with ID: " ++ show (t ^. threadId) ++ " has unspecified deadlines (inbound,outbound): " ++ show (dIn,dOut)

inferDeadline :: [Edge] -> Vertex -> (Deadline,Deadline)
inferDeadline es v = ( case dlsOut of {[] -> Infinity ; (x:_) -> Exact x}
                     , case dlsIn of {[] -> Infinity ; (x:_) -> Exact x}
                     )
  where
    vId    = v_id v
    dlsOut = L.sort . catMaybes $ map deadline (filter ((== vId) . start) es)
    dlsIn  = reverse  . L.sort . catMaybes $ map deadline (filter ((== vId) . end) es)

traceMsg = lift . SoOSiM.traceMsg

partitionHashMap f t = (HashMap.filter f t, HashMap.filter (not . f) t)
