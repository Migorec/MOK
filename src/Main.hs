module Main where

import Model.Model
import Model.Imitational

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

main =  do res <- imitate testParam
           print res
