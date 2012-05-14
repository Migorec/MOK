module Main where

import Graphics.UI.WX
import Model.Model hiding (set)
import Model.Imitational
import Model.Theoretical

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
         
         set f [layout := margin 10 (column 10 
                                       [label "Параметры системы:",
                                        (row 0 [label "K=", widget inpK, label "λ=",widget inpLambda ]),
                                        (row 0 [label "M=", widget inpM, label "μ=",widget inpMu ]),
                                        (row 0 [label "N=", widget inpN, label "ν=",widget inpNu ]),
                                        (row 0 [label "α=", widget inpAlpha, label "β=",widget inpBeta ]),
                                        (row 0 [label "γ=", widget inpGamma, label "δ=",widget inpDelta ])
                                       ])]
