{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
module SoOSiM.Components.PeriodicIO where

import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Control.Monad
import Data.Maybe

import SoOSiM
import SoOSiM.Components.Common
import SoOSiM.Components.Scheduler

newtype PeriodicIO = PeriodicIO String

newtype PeriodicIOS = PeriodicIOS ( TVar [(TQueue Int,Int,Int,Int)]
                                  , [(TQueue Int,Int,String,Int)]
                                  , ComponentId
                                  )

data PIO_Cmd = PIO_Stop
  deriving Typeable

instance ComponentInterface PeriodicIO where
  type State PeriodicIO               = PeriodicIOS
  type Receive PeriodicIO             = PIO_Cmd
  type Send PeriodicIO                = ()
  initState                           = const (PeriodicIOS (undefined,[],(-1)))
  componentName (PeriodicIO an) = ("<<" ++ an ++ ">>Periodic IO")
  componentBehaviour                  = const periodicIO

periodicIO ::
  PeriodicIOS
  -> Input PIO_Cmd
  -> Sim PeriodicIOS
periodicIO _ (Message _ PIO_Stop _) = stop

periodicIO s@(PeriodicIOS (qsS,ds,sId)) _ = do
  currentTime <- getTime
  qs  <- runSTM $ readTVar qsS
  qs' <- fmap catMaybes $ forM qs $ \(q,c,p,n) -> do
            case n of
              0 -> return Nothing
              n | c == (p-1) -> do runSTM $ writeTQueue q currentTime
                                   newIOToken sId
                                   return $ Just (q,0,p,n-1)
                | otherwise  -> return $ Just (q,c+1,p,n)
  runSTM $ writeTVar qsS qs'

  forM_ ds $ \(q,n,fN,tid) ->
    untilNothing (runSTM $ tryReadTQueue q)
                 (\d -> when ((currentTime - d) > n) (deadLineMissed d currentTime n fN tid)
                 )

  return s

stopPIO :: ComponentId -> String -> Sim ()
stopPIO cId n = notify (PeriodicIO n) cId PIO_Stop

deadLineMissed st et n fN tid =
  traceMsgTag (appThread ++ " missed deadline of " ++ show n ++ " by " ++ show (et - st - n) ++ " cycles")
              ("DeadlineMissed " ++ appThread ++ " " ++ show missed)
  where
    missed = et - st - n
    appThread = fN ++ ".T" ++ show tid

