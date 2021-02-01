module Barnsley
-- ( basicBarnsley
-- , sim
-- )
where

import Types
import System.Random
import Data.Matrix

{- Barnsley leaf -}

basicBarnsley = ()

-- This is definitely overkill; thought it would be nice to try the Matrix module
f1 = fromList 2 2 [0, 0, 0, 0.16]
f2 = fromList 2 2 [0.85, 0.04, -0.04, 0.85]
f3 = fromList 2 2 [0.20, -0.26, 0.23, 0.22]
f4 = fromList 2 2 [-0.15, 0.28, 0.26, 0.24]

a1 = zero 2 1 :: Matrix Double
a2 = fromList 2 1 [0.0, 1.6]
a3 = a2
a4 = fromList 2 1 [0.0, 0.44]

transform :: Matrix Double -> Matrix Double -> Pos -> Pos
transform m a (xPos, yPos) = (x', y')
  where
    p = multStd m $ fromList 2 1 [xPos, yPos]
    [x', y'] = toList $ p + a

mult :: Int -> Pos -> Pos
mult 1 = transform f1 a1
mult n | n <= 86 = transform f2 a2
mult n | n <= 93 = transform f3 a3
mult _ = transform f4 a4

sim :: () -> Int -> Int -> [Pos]
sim _ seed n =
  let gen = mkStdGen seed in
  sim_ nextPos () gen n [(0, 0)]

nextPos () gen curr = (new, newGen)
  where
    (v, newGen) = uniformR (1 :: Int, 100 :: Int) gen
    new = mult v curr