module Types where

import System.Random

type XPos = Double
type YPos = Double
type Pos = (XPos, YPos)

sim_ :: (s -> StdGen -> Pos -> (Pos, StdGen)) -> s -> StdGen -> Int -> [Pos] -> [Pos]
sim_ _ _ _ 0 ps = ps
sim_ f s gen n (curr:rest) =
  sim_ f s newGen (n - 1) ps where
    (new, newGen) = f s gen curr
    ps = new:curr:rest