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
          ms = reverse [1..fromIntegral $ m param ]
          summands = map (\j -> (alpha param)^j * 
                                (product $ take j ms) / 
                                (product $ take j betas) ) [1..m param]
          p0 = 1 / (1 + (sum summands))

chanProb :: MParam -> [Double] -> [Double]
chanProb param pP = pi0:(map (pi0*) summands)
    where deltas = map (\j -> delta param * (sum $ zipWith (\p i -> p * j / (i+j) 
                                                                     * min (i+j) (fromIntegral $ l param))
                                                         pP [0 .. fromIntegral $ m param])
                      ) [1.. fromIntegral $ n param]
          ns = reverse [1..fromIntegral $ n param]
          summands = map (\j -> (gamma param)^j * 
                                (product $ take j ns) / 
                                (product $ take j deltas) ) [1..n param]
          pi0 = 1 / (1 + (sum summands))


breakProb :: MParam -> ([Double],[Double])
breakProb param = iter initVal
    where initVal = replicate (n param) (1 / (fromIntegral $ n param))--1 : (replicate (n param ) 0) --replicate (n param) (1 / (fromIntegral $ n param))
          iter cP = let pP' = procProb param cP
                        cP' = chanProb param pP'
                    in  if (maximum $ zipWith (\p p' -> abs $ p - p') cP cP') < 0.01
                        then (pP',cP')
                        else iter cP' 
                        
                        
xi :: MParam -> ([Double],[Double]) -> Int  -> Double
xi param (ps,pis) num = sum $ zipWith (*) hatPs mus
    where
          mus = map (\i -> mu param * (sum $ zipWith (\p j -> p * (min i (fromIntegral $ m param - j))) 
                                                   ps [0.. fromIntegral $ m param])) 
                    [1 .. fromIntegral num] 
          nus = map (\i -> nu param * (sum $ zipWith (\pi j -> pi * (min (fromIntegral num - i + 1) 
                                                                         (fromIntegral $ n param - j))) 
                                                   pis [0.. fromIntegral $ n param])) 
                    [1 .. fromIntegral num] 
          summands = map (\i -> (product $ take i nus) / (product $ take i mus)) [1 .. num]
          hatP0 = 1/(1 + (sum summands))
          hatPs = map (hatP0 *) summands
          
xiStar :: MParam -> [Double] -> Double
xiStar param xis = sum $ zipWith (*) xis hatPis
    where ks = reverse [1 .. fromIntegral $ k param ]
          summands = map (\i -> (lambda param)^i * 
                                (product $ take i ks) / 
                                (product $ take i xis)) [1..fromIntegral $ k param]
          hatPi0 = 1/(1 + (sum summands))
          hatPis = map (hatPi0 *) summands
          
solve :: MParam -> Double
solve param = xiStar param xis
    where (ps,pis) = breakProb param
          xis = map (xi param (ps,pis)) [1 .. k param]
