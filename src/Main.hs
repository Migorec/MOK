module Main where

import Model.Model
import Model.Imitational
import Model.Theoretical 

testParam = MParam {m = 5, 
		            n = 1, 
		            k = 4, 
                    l=2, 
                    lambda = 3, 
                    mu = 2, 
                    nu = 1, 
                    alpha = 0.01, 
                    beta = 0.02,
                    gamma = 0.01,
                    delta = 0.02
                   }

main =  do let (ps,pis) = breakProb testParam 
           print (ps,pis)
           print $ map (xi testParam (ps,pis)) [1 .. k testParam]
           print $ solve testParam
           
           res <- imitate testParam
           print res
