-- Is using gloss, so should be compiled with -threaded option.

import Control.Arrow ((***))
import Data.Complex
import Debug.Trace
import Graphics.Gloss
import System.Random (RandomGen, getStdGen)

import qualified Balls as B
import Balls (Vec2)
import MotionVec (randomVecs)


modelHeight = 500
modelWidth  = 600

modelHeightf = fromIntegral modelHeight
modelWidthf  = fromIntegral modelWidth

modelH2 = fromIntegral modelHeight / 2
modelW2 = fromIntegral modelWidth / 2

modelTop = modelH2
modelBot = negate modelH2
modelLeft = negate modelW2
modelRight = modelW2


main = do
  g <- getStdGen
  let window = InWindow "Gloss Balls" (modelWidth, modelHeight) (0,0)
  let initModel = makeRandomModel g 10
--  display window white (drawModel $ makeRandomModel g 10)
  simulate
        window           -- display
        white            -- background
        30               -- steps per second
        initModel        -- initial model
        drawModel        -- display function
        (\v -> stepModel) -- step function

------------------------------------------------------------

data Ball = Ball { ballPos, ballVec :: Vec2 Float } deriving Show

ballSize = 30

drawBall :: Ball -> Picture
drawBall (Ball (x :+ y) v) =
    translate x y $ Color red $ circleSolid ballSize

stepBall :: Float -> Ball -> Ball
stepBall dt (Ball p@(x:+y) v@(dx:+dy)) =
    let dx' = if x < modelLeft || modelRight < x then -dx else dx
        dy' = if y < modelBot || modelTop < y then -dy else dy
        v'  = dx' :+ dy'
    in Ball (p + vmult dt v') v'

vmult s (x :+ y) = s*x :+ s*y

------------------------------------------------------------

type Model = [Ball]

drawModel :: Model -> Picture
drawModel = Pictures . (rect :) . map drawBall where
--    rect = translate w2 h2 . Color black $ rectangleWire modelWidthf modelHeightf
    rect = Color black $ rectangleWire modelWidthf modelHeightf
    tr = translate (-w2) (-h2)
    w2 = modelWidthf / 2
    h2 = modelHeightf / 2

stepModel :: Float -> Model -> Model
stepModel dt = map (stepBall dt) . collissions bcp bcf 
    where
    bcf (Ball p v) (Ball q u) = (Ball p *** Ball q) (B.collideBalls p v q u)
    bcp (Ball p v) (Ball q u) =
        let d1 = B.distance p q
            d2 = B.distance (p + vmult dt v) (q + vmult dt u)
        in B.isCollission p ballSize q ballSize && (d2 < d1)



initModel = [ Ball (50:+50) (40:+20)
            , Ball (100:+150) ((-20):+(-30))
            , Ball (100:+80) (20:+(-50))
            , Ball (200:+200) ((-20):+(-50))
            , Ball (30:+200) (50:+(-30))
            ]

makeRandomModel :: (RandomGen g) => g -> Int -> Model
makeRandomModel g n = zipWith Ball ps vs where
    vs = randomVecs g 10 80
    as = [fromIntegral i * a | i <- [1..n]]
    ps = [mkPolar s a | a <- as]
    d  = 2*ballSize + 10
    s  = (d * fromIntegral n)/(2*pi)
    a  = d/s

------------------------------------------------------------

-- | Detecting and handling collissions between objects.
collissions :: Show a =>        -- ^ only for debug
              (a -> a -> Bool)  -- ^ function for detecting collission
            -> (a -> a -> (a,a)) -- ^ function to apply when collission
            -> [a]             -- ^ objects
            -> [a]             -- ^ updated objects -- not necessarily in order
collissions collides cf = trav where
    trav (x:xs) = test x xs
    trav []     = []
    test x xs   = x' : trav xs' where
        (x',xs') = foldl f (x,[]) xs
        f (x,as) y | collides x y = {-traceShow (x,y) $ -} let (x',y') = cf x y in (x',y':as)
                   | otherwise    = (x,y:as)

