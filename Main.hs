module Main where

import qualified Sierpinski as S
import qualified Barnsley as B
import Types
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

main :: IO ()
main = do
  output "Sierpinski Plot" 42 100000 S.basicSierpinski S.sim
  output "Barnsley Plot" 59 100000 B.basicBarnsley B.sim

output title seed niterations config sim = toFile def fname $ do
    layout_title .= title
    plot (points "" ps)
    where fname = title ++ "_" ++ (show seed) ++ "_" ++ (show niterations) ++ ".png"
          ps = sim config seed niterations