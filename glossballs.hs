-- Is using gloss, so should be compiled with -threaded option.

import Control.Arrow ((***))
import Data.Complex
import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Interface.Environment (getScreenSize)
import System.Random (RandomGen, getStdGen)
import ReadArgs      (readArgs)

import qualified Balls as B
import Balls (Vec2)
import MotionVec (randomVecs)

initModelHeight, initModelWidth :: Num a => a
initModelHeight = 500
initModelWidth  = 600

modelH2 m = modelHeight m / 2
modelW2 m = modelWidth m / 2

modelTop m = modelH2 m
modelBot m = negate (modelH2 m)
modelLeft m = negate (modelW2 m)
modelRight m = modelW2 m


main = do
  (n, fs) <- readArgs  -- ^ number of balls in model, and fullscreen
  let fullScreen = fs == Just True
  g <- getStdGen
  let balls = randomBalls g n
  let window = if fullScreen
      then FullScreen
      else InWindow "Gloss Balls" (initModelWidth, initModelHeight) (0,0)
  (w,h) <- getScreenSize
  let modelWidth = if fullScreen then fromIntegral w else initModelWidth
  let modelHeight = if fullScreen then fromIntegral h else initModelHeight
  let initModel = Model { modelWidth = modelWidth
                        , modelHeight = modelHeight
                        , modelBalls = randomBalls g n
                        }
  simulate
        window           -- display
        (dark $ dark $ dark blue) -- background
        30               -- steps per second
        initModel        -- initial model
        drawModel        -- display function
        (const stepModel) -- step function

------------------------------------------------------------

data Ball = Ball { ballPos, ballVec :: Vec2 Float } deriving Show

ballSize = 30

drawBall :: Ball -> Picture
drawBall (Ball (x :+ y) v) =
    translate x y $ Color white $ circleSolid ballSize

stepBall :: Model -> Float -> Ball -> Ball
stepBall m dt (Ball p@(x:+y) v@(dx:+dy)) =
    let dx' = if x-s < modelLeft m || modelRight m < x+s then -dx else dx
        dy' = if y-s < modelBot m  || modelTop m   < y+s then -dy else dy
        v'  = dx' :+ dy'
        s   = ballSize
    in Ball (p + vmult dt v') v'

vmult s (x :+ y) = s*x :+ s*y

------------------------------------------------------------

data Model = Model { modelWidth, modelHeight :: Float, modelBalls :: [Ball] }

drawModel :: Model -> Picture
drawModel m = Pictures . (rect :) . map drawBall $ modelBalls m where
    rect = Color black $ rectangleWire (modelWidth m) (modelHeight m)


stepModel :: Float -> Model -> Model
stepModel dt m = m { modelBalls = map (stepBall m dt) . collissions bcp bcf $ modelBalls m }
    where
    bcf (Ball p v) (Ball q u) = (Ball p *** Ball q) (B.collideBalls p v q u)
    bcp (Ball p v) (Ball q u) =
        let d1 = B.distance p q
            d2 = B.distance (p + vmult dt v) (q + vmult dt u)
        in B.isCollission p ballSize q ballSize && (d2 < d1)


randomBalls :: (RandomGen g) => g -> Int -> [Ball]
randomBalls g n = zipWith Ball ps vs where
    vs = randomVecs g 10 80
    ps = radialBallPoints ballSize 10 n


-- | Calculate the points to radially position 'n' circles with radius
-- 'r' such that the distance between two balls is 'd'. Actually, this
-- is the same as calculating the vertices of a regular n-sided
-- polygon with sides '2*r+d'.
radialBallPoints
  :: (Integral n, RealFloat a) => a -> a -> n -> [Complex a]
radialBallPoints r d 1 = [0 :+ 0]
radialBallPoints r d n = map (mkPolar $ circumradius n s) as where
    as = [fromIntegral i * a | i <- [1..n]]
    n' = fromIntegral n
    a  = 2*pi / n'
    s  = 2*r + d


-- | The radius of a regular polygon with n sides of length s.
circumradius :: (Floating a, Integral n) => n -> a -> a
circumradius n s = s / (2*sin (pi / fromIntegral n))


------------------------------------------------------------

-- | Detecting and handling collissions between objects.
collissions :: (a -> a -> Bool)  -- ^ function for detecting collission
            -> (a -> a -> (a,a)) -- ^ function to apply when collission
            -> [a]             -- ^ objects
            -> [a]             -- ^ updated objects -- not necessarily in order
collissions collides cf = trav where
    trav (x:xs) = test x xs
    trav []     = []
    test x xs   = x' : trav xs' where
        (x',xs') = foldl f (x,[]) xs
        f (x,as) y | collides x y = let (x',y') = cf x y in (x',y':as)
                   | otherwise    = (x,y:as)
