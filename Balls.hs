module Balls 
 ( collideBalls
 , isCollission
 , distance
 , Vec2
 )
where

import Data.Complex


type Vec2 = Complex


angle :: RealFloat a => Vec2 a -> Vec2 a -> a
angle (a:+b) (x:+y) = atan2 y x - atan2 b a


-- | (components u v) gives the component vectors of 'v' relative 'u'.
components
  :: RealFloat t => Vec2 t -> Vec2 t -> (Vec2 t, Vec2 t)
components u v = (c1,c2) where
    c1    = mkPolar l (phase u)
    c2    = v - c1
    l     = magnitude v * cos alpha
    alpha = phase v - phase u
    

-- | (collideBalls p u q v) calculates new motion vectors given a
-- collission of two balls, one at 'p' with motion 'u', the other at
-- 'q' with motion 'v'.
collideBalls
  :: RealFloat a
  => Vec2 a -- ^ position of ball
  -> Vec2 a -- ^ motion vector of ball
  -> Vec2 a -- ^ position of other ball
  -> Vec2 a -- ^ motion vector of other ball
  -> (Vec2 a, Vec2 a)
collideBalls p u q v = (u',v') where
    u'      = v1 + u2
    v'      = u1 + v2
    (u1,u2) = components (q-p) u
    (v1,v2) = components (p-q) v
    c       = mkPolar 1 (angle p q)


mkVec s a = mkPolar s (a*pi/180)


-- | (isCollission p r q s) returns True iff two balls, one at 'p'
-- with radius 'r', the other at 'q' with radius 's', are within
-- collission distance.
isCollission
  :: (Floating a, Ord a) => Vec2 a -> a -> Vec2 a -> a -> Bool
isCollission p r q s = distance p q < r + s

distance :: Floating a => Vec2 a -> Vec2 a -> a
distance (a:+b) (x:+y) = sqrt((a-x)^2 + (b-y)^2)


normalise v = mkPolar 1 (phase v)


-- Does not work:
--components2 b (x :+ y) = ((x:+0)*v, (0:+y)*v) where v = normalise b

vector l a = mkPolar l (a*pi/180)
