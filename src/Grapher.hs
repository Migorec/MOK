module Main where

import Model.Model
import Model.Imitational
import Model.Theoretical
import System
import Control.Monad(liftM)
import Graphics.Gnuplot.Simple
import Graphics.Gnuplot.Terminal.SVG
import Prelude hiding (init)


--f :: Param -> MParam -> Double -> Double
--f p b x = solve $ set p b x 

main :: IO ()
main = do (inp:out:t) <- getArgs
          (EParam b p i f h) <- read `liftM` readFile inp
          let r = [i,i+h .. f]
          let p1=map (\x -> (x,solve $ set p b x)) r
          p2 <- mapM (\x -> do res <- imitate $ set p b x
                               return (x,res)
                     ) r 
          let st1 = defaultStyle {lineSpec = CustomStyle [LineTitle ""]}
              st2 = defaultStyle {lineSpec = CustomStyle [LineTitle ""]}
          plotPathsStyle [terminal $ cons out] [(st1,p1),(st2,p2)]
          
          
