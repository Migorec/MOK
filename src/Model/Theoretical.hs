module Model.Theoretical where

import Model.Model

{-
data MParam = MParam { m :: Int, -- Число процессоров
		               n :: Int,  -- Число каналов
		               k  :: Int, -- Число задач
                       l  :: Int, -- Число ремонтных бригад
                       lambda :: Double, -- Интенсивность обдумывания
                       mu :: Double, -- Интенсивность обработки на процессороной фазе
                       nu :: Double, -- Интенсивность обработки на канальной фазе
                       alpha :: Double, -- Интенсивность отказа процессоров
                       beta :: Double, -- Интенсивность восстановления процессоров
                       gamma :: Double, -- Интенсивность отказа каналов
                       delta :: Double -- Интенсивность восстановления каналов
                      }
-}

procProb :: MParam -> [Double] -> [Double]
procProb param cP = p0:(map (p0*) summands)
    where betas = map (\i -> beta param * (sum $ zipWith (\pi j -> pi * i / (i+j) 
                                                                      * min (i+j) (fromIntegral $ l param))
                                                         cP [0 .. fromIntegral $ n param])
                      ) [1.. fromIntegral $ m param]
          ms = [fromIntegral $ m param .. 1]
          summands = map (\j -> (alpha param)^j * 
                                (product $ take j ms) / 
                                (product $ take j betas) ) [1..m param]
          p0 = 1 / (1 + (sum summands))

chanProb :: MParam -> [Double] -> [Double]
chanProb param pP = pi0:(map (pi0*) summands)
    where deltas = map (\j -> beta param * (sum $ zipWith (\p i -> p * j / (i+j) 
                                                                     * min (i+j) (fromIntegral $ l param))
                                                         pP [0 .. fromIntegral $ m param])
                      ) [1.. fromIntegral $ n param]
          ns = [fromIntegral $ n param .. 1]
          summands = map (\j -> (alpha param)^j * 
                                (product $ take j ns) / 
                                (product $ take j deltas) ) [1..n param]
          pi0 = 1 / (1 + (sum summands))


breakProb :: MParam -> ([Double],[Double])
breakProb param = iter initVal
    where initVal = replicate (n param) (1 / (fromIntegral $ n param))
          iter cP = let pP' = procProb param cP
                        cP' = chanProb param pP'
                    in  if (maximum $ zipWith (\p p' -> abs $ p - p') cP cP') < 0.01
                        then (pP',cP')
                        else iter cP' 
