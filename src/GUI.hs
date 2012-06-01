module Main where

import Graphics.UI.WX
import Model.Model hiding (set)
import Model.Imitational
import Model.Theoretical
import Control.Monad (zipWithM)
import qualified Control.Exception as E

main :: IO ()
main = start gui

gui = do f <- frame [text := "МОК: Вариант 17-12"]
         inpK <- entry f [text := "1" , enabled := True ]
         inpM <- entry f [text := "1" , enabled := True ]
         inpN <- entry f [text := "1" , enabled := True ]
         inpL <- entry f [text := "0" , enabled := True ]
         inpLambda <- entry f [text := "1" , enabled := True ]
         inpMu <- entry f [text := "1" , enabled := True ]
         inpNu <- entry f [text := "1" , enabled := True ]
         inpAlpha <- entry f [text := "0" , enabled := True ]
         inpBeta <- entry f [text := "0" , enabled := True ]
         inpGamma <- entry f [text := "0" , enabled := True ]
         inpDelta <- entry f [text := "0" , enabled := True ]
         btnCalc <- button f [text := "Расчитать", on command := E.catch (do par <- getMParam 
                                                                                  [inpK,inpM,inpN,inpL]
                                                                                  ["K","M","N","L"]
                                                                                  [inpLambda, inpMu, inpNu,
                                                                                   inpAlpha, inpBeta,
                                                                                   inpGamma, inpDelta]
                                                                                  ["λ", "μ", "ν", "α", 
                                                                                   "β", "γ", "δ"]
                                                                             i <- imitate par
                                                                             let t = solve par
                                                                             result f t i)
                                                                          (\e -> errorDialog f "Ошибка" (show (e :: E.IOException)))]
         let mv3 = (marginWidth 3).marginTop
             c1 = fill $ margin 3 $ column 10 [mv3 $ label "K=",
                            label "M=",
                            label "N=",
                            label "α=",
                            label "γ=",
                            label "L="]
             c2 = fill $ margin 3 $ column 0 [widget inpK,
                            widget inpM,
                            widget inpN,
                            widget inpAlpha,
                            widget inpGamma,
                            widget inpL]
             c3 = fill $ margin 3 $ column 10 [mv3 $ label "λ=",
                            label "μ=",
                            label "ν=",
                            label "β=",
                            label "δ="]
             c4 = fill $ margin 3 $ column 0 [widget inpLambda,
                            widget inpMu,
                            widget inpNu,
                            widget inpBeta,
                            widget inpDelta,
                            widget btnCalc]

        {- set f [layout := fill $ marginTop  (column 10 
                  [fill $ label "Параметры системы:",
                   fill $ row 1 [ marginWidth 3 $ marginTop $ label "K=",widget inpK,glue,margin 3 $ label "λ=",widget inpLambda],
                   fill $ row 1 [ marginTop $ marginTop $ label "M=",widget inpM,glue,margin 3 $ label "μ=",widget inpMu],
                   fill $ row 1 [ margin 3 $ label "N=",widget inpN,glue,margin 3 $ label "ν=",widget inpNu],
                   fill $ row 1 [ margin 3 $ label "α=",widget inpAlpha,glue,margin 3 $ label "β=",widget inpBeta],
                   fill $ row 1 [ margin 3 $ label "γ=",widget inpGamma,glue,margin 3 $ label "δ=",widget inpDelta],
                   fill $ row 1 [ margin 3 $ label "L=",widget inpL,glue,widget btnCalc]
                                        ])]-}
                                        
         set f [layout := fill $ marginTop (column 10 
                        [fill $ label "Параметры системы:",
                         fill $ row 1 [c1,c2,glue,c3,c4]])]
                         
getMParam w1 s1 w2 s2 = do [kv,mv,nv,lv] <- zipWithM getInt w1 s1
                           [lambdav,muv,nuv,alphav,betav,gammav,deltav] <- zipWithM getDbl w2 s2
                           return $ MParam { m = mv,
		                                     n = nv,
		                                     k = kv,
                                             l = lv,
                                             lambda = lambdav,
                                             mu = muv,
                                             nu = nuv,
                                             alpha = alphav,
                                             beta = betav,
                                             gamma = gammav,
                                             delta = deltav
                                            }                         
                         
getInt w s1 = do s <- get w text
                 case reads s :: [(Int,String)] of
                    [(val,"")] -> return val
                    _ -> fail ("Значение " ++ s1 ++ " должно быть целым числом")

getDbl w s1 = do s <- get w text
                 case reads s :: [(Double,String)] of
                    [(val,"")] -> return val
                    _ -> fail ("Значение " ++ s1 ++ " должно быть числом")
                    
result :: Frame () -> Double -> Double -> IO ()
result f t i = do d <- dialog f [text := "Результаты расчетов"]
                  ok <- button d [text := "Ok"]
                  set d [layout := margin 3 $ column 3 [label "Производительность системы",
                                                        label $ "Теоретическая модель: " ++ show t,
                                                        label $ "Имитационная модель: " ++ show i,
                                                        widget ok]]
                  res <- showModal d (\stop -> set ok [on command := stop (Just 42)])
                  return ()
