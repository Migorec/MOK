module Main where

import Model.Model
import Model.Imitational
import Model.Theoretical 

tp1 = MParam {      m = 1, 
		            n = 1, 
		            k = 2, 
                    l=1, 
                    lambda = 1, 
                    mu = 5, 
                    nu = 5, 
                    alpha = 0.0, 
                    beta = 0.1,
                    gamma = 0.0,
                    delta = 0.1
                   }

testParam = MParam {m = 5, 
		            n = 1, 
		            k = 10, 
                    l=2, 
                    lambda = 3, 
                    mu = 2, 
                    nu = 1, 
                    alpha = 0.01, 
                    beta = 0.02,
                    gamma = 0.01,
                    delta = 0.02
                   }

main =  do 
           --print $ breakProb tp1
           --print $ map (xi tp1 (breakProb tp1)) [1..5]
           print $ solve testParam
           
           res <- imitate testParam
           print res
