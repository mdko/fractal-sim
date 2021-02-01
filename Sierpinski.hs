module Sierpinski 
( Sierpinski(..)
, basicSierpinski
, sim
) where

{- Sierpinksi triangle -}

import Types
import System.Random

data Sierpinski = Sierpinski
  { posA :: Pos,
    posB :: Pos,
    posC :: Pos
  }

basicSierpinski :: Sierpinski
basicSierpinski = Sierpinski (0, 0) (1000, 0) (500, 866)

halfWay :: Pos -> Pos -> Pos
halfWay (srcX, srcY) (dstX, dstY) =
  let newX = (dstX + srcX) / 2 in
  let newY = (dstY + srcY) / 2 in
  (newX, newY)

sim :: Sierpinski -> Int -> Int -> [Pos]
sim s seed n =
    let gen = mkStdGen seed in
    sim_ nextPos s gen n [(0, 0)]

nextPos s gen curr =
  let (v, newGen) = uniformR (1 :: Int, 6 :: Int) gen in
  let dest = case v of 1 -> posA s
                       2 -> posA s
                       3 -> posB s
                       4 -> posB s
                       5 -> posC s
                       6 -> posC s in
  let new = halfWay curr dest in
  (new, newGen)