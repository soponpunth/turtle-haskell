-- | proper module documentation here
module Turtle (
  -- * The turtle type(s)
  -- Non-exhaustive list of possible types: Turtle, Program, Action, Operation ...
    Program
  , Turtle
  , Action
  , PenState


  -- * Primitive operations
  , forward


  -- * Run functions
  , runTextual
  -- ...

  ) where

-- | Description of your type here...
--
--   You can use newtype instead of data if you wish.
data Action =   MoveForward Double
              | MoveBackward Double
              | TurnLeft Double
              | TurnRight Double
              | PenUp
              | PenDown
              | Die
              | Idle
              | SetColor Color
              deriving Show

data PenState = Up | Down

data Program = ProgramList Action Program | ProgramEnd deriving Show
type Position = (Double, Double)
type Angle = Double
type Color = (Double, Double, Double)

data Turtle = Turtle { program :: Program
                     , position :: Position
                     , angle :: Angle
                     , color :: Color
                     , penstate :: PenState
                     }

runTextual :: Program -> IO()
runTextual ProgramEnd                 = putStrLn "ProgramEnd"
runTextual (ProgramList action program) = do
                                          putStrLn $ show action 
                                          runTextual program                     



spiral :: Double -> Double -> Program
spiral s a = pendown >*> forward s >*>
             right a >*> spiral (s + 0.005) a

-- | Renders a finite spiral.
spiralLimited :: Integer -> Double -> Program
spiralLimited size angle = limited size $ spiral 0 angle
