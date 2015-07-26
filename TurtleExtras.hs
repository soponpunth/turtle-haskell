module TurtleExtras (
	square
	, sizeSquare
	, triangle
	, sizeTriangle
	, polygon'
	, cU
	, hU
	, aU
	, lU
	, mU
	, eU
	, rU
	, sU
	, chalmers
	, chalmersColor
	, parallelChalmers
	, drawSpiral
	, drawInfiniteSpiral
	, drawTree
	, drawLimitedSpiral


	) where

import Turtle

square :: Program
square = times 4 (forward 50 >*> right 90)

sizeSquare :: Double -> Program
sizeSquare size = times 4 (forward size >*> right 90)

triangle :: Program
triangle = times 3 (forward 50 >*> right 120)

sizeTriangle :: Double -> Program
sizeTriangle size = times 3 (forward size >*> right 120)

polygon' :: Double -> Integer -> Program
polygon' size num = times num (forward size >*> right angle)
				where angle = 360/(fromIntegral num)

circle :: Double -> Program
circle r = 	times 360 (forward (2*pi*r/360) >*> left 1) 

arc :: Double -> Double -> Program
arc angle r = times (round angle) (forward (2*pi*r/360) >*> left 1)

cU :: Double -> Program
cU r = 	penup >*> right 90 >*> forward (2*r) >*> right 90 >*>
		forward r >*> right 180 >*> arc 45 r >*> pendown >*>
		arc 270 r >*> penup >*> arc 45 r >*> forward r >*>
		right 90 >*> forward 20 >*> left 90

hU :: Double -> Program
hU r = 	pendown >*> right 180 >*> forward (2*r) >*> penup >*>
		backward r >*> left 90 >*> pendown >*> forward (2*r) >*>
		right 90 >*> forward r >*> backward (2*r) >*> penup >*>
		left 90 >*> forward 20 >*> left 90

aU :: Double -> Program
aU r = 	penup >*> right 90 >*> forward (r) >*> pendown >*> right 116.6 >*>
		forward edge >*> backward edge >*> left 53.2 >*> forward edge >*> 
		backward (edge/2) >*> right 116.6 >*> forward r >*> right 116.6 >*> 
		forward (edge/2) >*> penup >*> right 63.4 >*> forward r
		>*> forward 20 >*> left 90
			where
				edge = r*(sqrt 5)

lU :: Double -> Program
lU r = 	pendown >*> right 180 >*> forward (2*r) >*> left 90 >*>
		forward (1.5*r) >*> penup >*> left 90 >*> forward (2*r) >*>
		right 90 >*> 
		forward 20 >*> left 90

mU :: Double -> Program
mU r = 	pendown >*> right 180 >*> forward (2*r) >*> backward (2*r) >*>
		left 26.6 >*> forward edge >*> left 126.8 >*> forward edge >*>
		right 153.4 >*> forward (2*r) >*> backward (2*r) >*> penup >*>
		left 90 >*> 
		forward 20 >*> left 90
			where
				edge = r*(sqrt 5)

eU :: Double -> Program 
eU r = 	pendown >*> right 90 >*> forward (1.5*r) >*> backward (1.5*r) >*>
		right 90 >*> forward r >*> left 90 >*> forward (1.4*r) >*>
		backward (1.4*r) >*> right 90 >*> forward r >*> left 90 >*>
		forward (1.5*r) >*> penup >*> left 90 >*> forward (2*r) >*>
		right 90 >*> 
		forward 20 >*> left 90

rU :: Double -> Program 
rU r = 	pendown >*> right 180 >*> forward (2*r) >*> backward (2*r) >*>
		left 90 >*> forward r >*> penup >*> right 90 >*> forward r >*>
		left 90 >*> pendown >*> backward r >*> forward r >*> arc 180 (r/2) >*>
		penup >*> arc 180 (r/2) >*> right 63.4 >*> pendown >*> 
		forward (r*(sqrt 5)/2) >*> penup >*> left 153.4 >*> forward (2*r) >*>
		right 90 >*> forward (r/2) >*> 
		forward 20 >*> left 90

sU :: Double -> Program
sU r = 	penup >*> right 90 >*> forward r >*> right 90 >*> forward (r/2) >*>
		right 180 >*> pendown >*> arc 270 (r/2) >*> penup >*> right 180 >*>
		arc 90 (r/2) >*> pendown >*> arc 270 (r/2) >*> penup >*> right 180 >*>
		forward r >*> left 90 >*> forward r >*> right 90 >*> 
		forward 20 >*> left 90



chalmers :: Double -> Program
chalmers d = drawTree 150 >*> penup >*> left 90 >*> forward 100 >*> right 90 
			  >*> cU d >*>  hU d >*> aU d  >*> lU d >*>  mU d >*> eU d >*>  rU d  >*> sU d

chalmersColor :: Double -> Program
chalmersColor d =  penup >*> left 90 >*> forward 100 >*> right 90 
			 >*> penColor TRed >*>  cU d >*> penColor TBlack >*> hU d >*> penColor TCyan >*>  aU d 
			 >*> penColor TMagenta >*> lU d >*> penColor TYellow >*> mU d >*> penColor TBlack
			  >*> eU d >*> penColor TRed >*> rU d  >*> penColor TGreen >*> sU d

parallelChalmers :: Program
parallelChalmers = drawTree 150 <|> chalmersColor 20		  

-- Spiral examples
type Size = Double
drawSpiral :: Size -> Angle -> Program
drawSpiral size angle | size > 100 = ProgramEnd
drawSpiral size angle | otherwise = newProgram >*> forward size >*> right angle >*> drawSpiral (size + 2) angle

-- draw infinite spiral
drawInfiniteSpiral :: Size -> Angle -> Program
drawInfiniteSpiral size angle = newProgram 
                                >*> forward size >*> right angle >*> penColor TBlue 
                                >*> forward size >*> right angle >*> penColor TBlack 
                                >*> forward size >*> right angle >*> penColor TWhite 
                                >*> forward size >*> right angle >*> penColor TYellow 
                                >*> forward size >*> right angle >*> penColor TRed 
                                >*> forward size >*> right angle >*> penColor TGreen 
                                >*> forward size >*> right angle >*> penColor TCyan 
                                >*> forward size >*> right angle >*> penColor TMagenta 
                                >*> drawInfiniteSpiral (size + 2) angle



-- draw limited spiral in term of the unlimited one
drawLimitedSpiral :: Integer -> Size -> Angle -> Program
drawLimitedSpiral n size angle = limited n $ drawInfiniteSpiral size angle

drawTree :: Size -> Program
drawTree size | size > 5  = newProgram 
                            >*> forward (size/3) 
                            >*> left 30 >*> drawTree (size*2/3) >*> right 30 
                            >*> forward (size/6)  
                            >*> right 25 >*> drawTree (size/2) >*> left 25
                            >*> forward (size/3)  
                            >*> right 25 >*> drawTree (size/2) >*> left 25
                            >*> forward (size/6) >*> backward size
              | otherwise = idle
