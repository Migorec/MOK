module Model.Imitational where

import Random
import Control.Monad
import Control.Monad.Trans
import Simulation.Aivika.Dynamics
import Simulation.Aivika.Dynamics.Base
import Simulation.Aivika.Dynamics.Lift
import Simulation.Aivika.Dynamics.EventQueue
import Simulation.Aivika.Dynamics.Ref
import Simulation.Aivika.Dynamics.Resource
import Simulation.Aivika.Dynamics.Process
import Model.Model


exprnd :: Double -> IO Double
exprnd lambda = do x <- getStdRandom random
                   return (- log x / lambda)

specs = Specs { spcStartTime = 0.0,
                spcStopTime = 1000.0,
                spcDT = 1.0,
                spcMethod = RungeKutta4 }

-- Функция моделирует отказы и восстановления процессоров и каналов.
-- Возвращает списоки интервалов времени в течение которых процессоро и каналы недоступны.
breakModel :: MParam -> Dynamics (Dynamics ([(Int,Double,Double)],[(Int,Double,Double)]))
breakModel param = do queue <- newQueue

                      pBreaks <- newRef queue []
                      cBreaks <- newRef queue []

                      repairers <- newResource queue $ l param
                      ppids <- replicateM (m param) $ newProcessID queue
                      cpids <- replicateM (n param) $ newProcessID queue

                      let proc :: Int -> Process ()
                          proc i = do t <- liftD time
                                      workTime <- liftIO $ exprnd $ alpha param
                                      holdProcess workTime
                                    
                                      requestResource repairers
                                      repairTime <- liftIO $ exprnd $ beta param
                                      holdProcess repairTime
                                      releaseResource repairers
                                  
                                      liftD $ modifyRef pBreaks ((i,t + workTime,t + workTime + repairTime):)
                                      proc i

                          chan :: Int -> Process ()
                          chan i = do t <- liftD time
                                      workTime <- liftIO $ exprnd $ gamma param
                                      holdProcess workTime
                                    
                                      requestResource repairers
                                      repairTime <- liftIO $ exprnd $ delta param
                                      holdProcess repairTime
                                      releaseResource repairers
                                  
                                      liftD $ modifyRef cBreaks ((i,t + workTime,t + workTime + repairTime):)
                                      chan i  

                      t0 <- starttime

                      zipWithM_ (\i p -> runProcess (proc i) p t0)   [1..m param] ppids
                      zipWithM_ (\i p -> runProcess (chan i) p t0) [1..n param] cpids

                      let system :: Dynamics ([(Int,Double,Double)],[(Int,Double,Double)])
                          system = do p <- readRef pBreaks
                                      c <- readRef cBreaks
                                      return $ (reverse p, reverse c)

                      return system     

-- Функция вычисляет время обработки заявки с учетом отказов
countTime :: Double  -- время начала обработки 
          -> Double  -- номинальное время обработки
          -> [(Double,Double)]  --расписане отказов
          -> Double  -- время обработки с учетом отказов
countTime sTime dT repTimes = (rdt sTime dT $ dropWhile (\(x,y) -> y <= sTime) repTimes) - sTime
    where rdt a dT [] = a + dT
          rdt a dT ((x,y):xys) = let ddt = if x - a > 0 then x - a else 0
                                 in if dT > ddt
                                    then rdt y (dT - ddt) xys
                                    else a + dT  

-- Функция моделирует работу системы
-- Возвращает производительность системы
model :: MParam -> ([(Int,Double,Double)],[(Int,Double,Double)]) -> Dynamics (Dynamics Double)
model param (ps,cs)= 
              do queue <- newQueue
                 
                 workTime <- newRef queue 0.0
                 reqDone <- newRef queue 0
                 procAvaliable <- newRef queue [1..m param]
                 chanAvaliable <- newRef queue [1..n param]

                 procs <- newResource queue $ m param
                 channels <- newResource queue $ n param
                 
                 pids <- replicateM (k param) $ newProcessID queue
                 
                 let request :: Process()
                     request = do thinkTime <- liftIO $ exprnd $ lambda param
                                  holdProcess thinkTime
                                  startTime <- liftD time
                                  
                                  requestResource procs
                                  
                                  t <- liftD time
                                  pr <- liftD $ readRef procAvaliable
                                  liftD $ writeRef procAvaliable $ tail pr
                                  let i = head pr
                                  
                                  procTime <- liftIO $ exprnd $ mu param
                                  
                                  holdProcess $ countTime t procTime $ map (\(_,x,y) -> (x,y)) $ 
                                                                       filter (\(j,_,_) -> j == i) ps 
                                  releaseResource procs
                                  liftD $ modifyRef procAvaliable (i:)

                                  requestResource channels
                                  
                                  t <- liftD time
                                  ch <- liftD $ readRef chanAvaliable
                                  liftD $ writeRef chanAvaliable $ tail ch
                                  let i = head ch
                                  
                                  channelTime <- liftIO $ exprnd $ nu param
                                  
                                  
                                  holdProcess $ countTime t channelTime $ map (\(_,x,y) -> (x,y)) $
                                                                          filter (\(j,_,_) -> j == i) cs
                                  releaseResource channels
                                  liftD $ modifyRef chanAvaliable (i:)
                                  
                                  endTime <- liftD time
                                  liftD $ modifyRef reqDone (+1)
                                  liftD $ modifyRef workTime (+ (endTime - startTime))

                                  request

                 t0 <- starttime

                 mapM_ (\p -> runProcess request p t0) pids

                 let system :: Dynamics Double
                     system = do x <- readRef workTime
                                 n <- readRef reqDone
                                 return $ (fromIntegral n) / x 

                 return system
                 
                      
imitate :: MParam -> IO Double
imitate p = do shed <- runDynamics1 (breakModel p) specs
               runDynamics1 (model p shed) specs
