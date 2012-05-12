-- {-# LANGUAGE TemplateHaskell, TypeOperators #-}

module Model.Model where


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
                      } deriving (Show,Read)
    
    
data Param = M | N | K | L | Lambda | Mu | Nu | Alpha | Beta | Gamma | Delta deriving (Read,Show)
             
--set :: =>  Param -> MParam -> Double -> MParam

set M p v = p{m=  round v }
set N p v = p{n=  round v }
set K p v = p{k=  round v }
set L p v = p{l=  round v }
set Lambda p v = p{lambda = v}
set Mu p v = p{mu = v}
set Nu p v = p{nu = v}
set Alpha p v = p{alpha = v}
set Beta p v = p{beta = v}
set Gamma p v = p{gamma = v}
set Delta p v = p{lambda = v}


     
data EParam = EParam { base :: MParam,
                       param :: Param,
                       init :: Double,
                       fin :: Double,
                       h :: Double
                      } deriving (Read, Show)
                      
-- $(mkLabels [''MParam])                      
                      
                      
                      
 
