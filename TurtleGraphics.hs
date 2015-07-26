-- | A graphical run function for the turtle DSL
module TurtleGraphics (runGraphical) where

import Turtle
import TurtleExtras
import Graphics.HGL


runGraphical :: Program -> IO ()
runGraphical (ProgramPLC tList)   =   runGraphics $ do
                                                      w <- openWindowEx "Turtle!" Nothing (1024, 1024) DoubleBuffered (Just 10)
                                                      drawInWindow w (polygon [(0,0),(0,1024),(1024,1024),(1024,0)])
                                                      onTick w $ getMLineGraphic tList
runGraphical p                    =   runGraphics $ do
                                                      w <- openWindowEx "Turtle!" Nothing (1024, 1024) DoubleBuffered (Just 10)
                                                      drawInWindow w (polygon [(0,0),(0,1024),(1024,1024),(1024,0)])
                                                      let newTurtle = initTurtle p
                                                      onTick w $ getLineGraphic newTurtle                  
                                                                   


getLineGraphic :: Turtle -> [Graphic]
getLineGraphic turtle = let
                            turtlePen = getPenstate turtle
                            turtleColor = getColorGraphic $ getColor turtle
                            (oldPos,newPos) = getLineTurtle turtle
                            newTurtle = updateTurtle turtle
                            graphic = withColor turtleColor   $ line  oldPos newPos
                          in
                           if turtlePen == Down then 
                              graphic : getLineGraphic newTurtle
                           else 
                              getLineGraphic newTurtle



getMLineGraphic :: [Turtle] -> [Graphic]
getMLineGraphic tList = let newTurtle = map updateTurtle tList                        
                        in
                          firstGraphic tList ++ getMLineGraphic newTurtle                           


firstGraphic :: [Turtle] -> [Graphic]
firstGraphic [] = []
firstGraphic (t:ts) =  let
                            turtlePen = getPenstate t
                            turtleColor = getColorGraphic $ getColor t
                            (oldPos,newPos) = getLineTurtle t
                            graphic = withColor turtleColor   $ line  oldPos newPos
                          in
                           if turtlePen == Down then 
                              graphic : firstGraphic ts
                           else 
                              firstGraphic ts  


getColorGraphic :: TColor -> Color 
getColorGraphic TBlack = Black
getColorGraphic TBlue = Blue
getColorGraphic TGreen = Green
getColorGraphic TCyan = Cyan
getColorGraphic TRed = Red
getColorGraphic TMagenta = Magenta
getColorGraphic TYellow = Yellow
getColorGraphic TWhite = White


onTick :: Window -> [Graphic] -> IO ()
onTick w []      = return ()
onTick w (x:xs)  = do
                    drawInWindow w x
                    onTick w xs
