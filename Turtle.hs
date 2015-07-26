module Turtle (
  
    Program(ProgramPLC,ProgramEnd)
  , Turtle
  , Action
  , PenState(Up,Down)
  , TColor(TBlack,TBlue,TGreen,TCyan,TRed,TMagenta,TYellow,TWhite)
  , Angle

  -- * Primitive operations
  , newProgram
  , initTurtle
  , updateTurtle
  , forward
  , backward
  , left
  , right
  , penup
  , pendown
  , penColor
  , idle
  , die
  , limited
  , lifespan
  , times
  , forever
  , (>*>)
  , (<|>)
  , getColor
  , getPenstate
  , getProgram
  , getLineTurtle

  -- * Run functions
  , runTextual

  ) where

-- datatype descriptions
data Action =   MoveForward Double
              | MoveBackward Double
              | TurnLeft Double
              | TurnRight Double
              | PenUp
              | PenDown
              | Die
              | Idle
              | SetColor TColor
              deriving Show

data PenState = Up | Down deriving (Show,Eq)

data Program = ProgramList Action Program | ProgramPLC [Turtle] | ProgramEnd | ProgramStop deriving Show
type Position = (Double, Double)
type Angle = Double
data TColor = TBlack | TBlue | TGreen | TCyan | TRed | TMagenta | TYellow | TWhite deriving Show 
type Line = ((Int,Int),(Int,Int))

data Turtle = Turtle { program :: Program
                     , position :: Position
                     , angle :: Angle
                     , color :: TColor
                     , penstate :: PenState
                     } deriving Show


-- initialize turtle
-- (need to change later about window coord in HGL)
initTurtle :: Program -> Turtle
initTurtle program = Turtle program (512.0,512.0) 270.0 TBlack Down

-- update current turtle state according to action
updateTurtle :: Turtle -> Turtle
updateTurtle (Turtle (ProgramList (MoveForward d) program)  pos angle color pen) = Turtle program (moveTurtle d pos angle) angle color pen                   
updateTurtle (Turtle (ProgramList (MoveBackward d) program) pos angle color pen) = Turtle program (moveTurtle (-d) pos angle) angle color pen                   
updateTurtle (Turtle (ProgramList (TurnLeft d) program)     pos angle color pen) = Turtle program pos (angle - d) color pen                   
updateTurtle (Turtle (ProgramList (TurnRight d) program)    pos angle color pen) = Turtle program pos (angle + d) color pen                   
updateTurtle (Turtle (ProgramList PenUp program)            pos angle color _  ) = Turtle program pos angle color Up                   
updateTurtle (Turtle (ProgramList PenDown program)          pos angle color _  ) = Turtle program pos angle color Down                   
updateTurtle (Turtle (ProgramList Die _program)             pos angle color pen) = Turtle ProgramStop pos angle color pen                   
updateTurtle (Turtle (ProgramList Idle program)             pos angle color pen) = Turtle program pos angle color pen                   
updateTurtle (Turtle (ProgramList (SetColor c) program)     pos angle color pen) = Turtle program pos angle c pen                   
updateTurtle (Turtle program                                pos angle color pen) = Turtle program pos angle color pen  



getLineTurtle :: Turtle -> Line
getLineTurtle turtle = let 
                            (x,y) = position turtle
                            (x',y') = position $ updateTurtle turtle
                            intPos = (round x,round y)
                            intnPos = (round x',round y')
                         in
                            (intPos,intnPos) 
-- move turtle's postion helper
moveTurtle :: Double -> Position -> Angle -> Position
moveTurtle d (x,y) angle = (x',y')
                      where 
                        angle' = angle*2*pi/360
                        x' = x + d * (cos angle')
                        y' = y + d * (sin angle')

getColor :: Turtle -> TColor
getColor = color 

getPenstate :: Turtle -> PenState
getPenstate  = penstate     

getProgram :: Turtle -> Program
getProgram = program

-- create new empty program
newProgram :: Program
newProgram = ProgramEnd

-- common operations for turtle
actionProgram :: Action -> Program
actionProgram a = ProgramList a ProgramEnd

forward :: Double -> Program
forward d = actionProgram (MoveForward d)

backward :: Double -> Program
backward d = actionProgram (MoveBackward d)

left :: Double -> Program
left d = actionProgram (TurnLeft d)

right :: Double -> Program
right d = actionProgram (TurnRight d)

penup :: Program
penup = actionProgram PenUp

pendown :: Program
pendown = actionProgram PenDown

penColor :: TColor -> Program
penColor c = actionProgram (SetColor c)

idle :: Program
idle = actionProgram Idle

die :: Program
die = actionProgram Die

limited :: Integer -> Program -> Program
limited 0 _                            = ProgramEnd
limited _ ProgramEnd                   = ProgramEnd
limited n (ProgramList action program) = ProgramList action (limited (n-1) program)

lifespan :: Integer -> Program -> Program
lifespan n program = limited n program >*> die

times :: Integer -> Program -> Program
times 0 _ = ProgramEnd
times n program = program >*> times (n-1) program

forever :: Program -> Program
forever program = program >*> forever program

(>*>) :: Program -> Program -> Program
ProgramEnd           >*> program = program
ProgramStop           >*> program = ProgramStop
(ProgramList Die program)  >*> program2 = ProgramStop
(ProgramList action program)  >*> program2 = ProgramList action (program >*> program2)

(<|>) :: Program -> Program -> Program
ProgramStop <|> q = q
ProgramEnd <|> q = q
p <|> ProgramStop = p
p <|> ProgramEnd = p
ProgramPLC tList <|> q  = ProgramPLC (tList ++ [initTurtle q])
p <|> ProgramPLC tList  = ProgramPLC ([initTurtle p] ++ tList)
p <|> q = ProgramPLC ([initTurtle p] ++ [initTurtle q])


-- print sequential steps for program
runTextual :: Program -> IO()
runTextual ProgramEnd                        = putStrLn "ProgramEnd"
runTextual ProgramStop                       = putStrLn "ProgramTerminated"
runTextual (ProgramList action program)      = do
                                               putStrLn $ show "Turtle : " ++ show action 
                                               runTextual program
runTextual (ProgramPLC [])                   = runTextual ProgramStop 
runTextual (ProgramPLC tList)               = do
                                                runFirstTextual tList
                                                let newTList = map updateTurtle tList 
                                                    isEmptyT = and (map isEmptyTurtle newTList)
                                                if not isEmptyT then
                                                  runTextual (ProgramPLC newTList)
                                                else 
                                                  putStrLn "---------------"

runFirstTextual :: [Turtle] -> IO()
runFirstTextual [] = putStrLn ""
runFirstTextual (t:ts) = do
                            let turProg = program t
                            case turProg of
                                (ProgramList a p) -> do
                                                      putStrLn $ show "Turtle : " ++ show a 
                                                      runFirstTextual ts
                                ProgramEnd        -> do
                                                      putStrLn $ show "ProgramEnd"
                                                      runFirstTextual ts       
                                ProgramStop        -> do
                                                      putStrLn $ show "ProgramTerminated"
                                                      runFirstTextual ts                                     
                 

isEmptyTurtle :: Turtle -> Bool
isEmptyTurtle (Turtle ProgramStop pos angle color pen) = True
isEmptyTurtle (Turtle ProgramEnd pos angle color pen) = True
isEmptyTurtle _ = False



