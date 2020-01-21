module Main where

import qualified Sort                          as S
import           Graphics.Gloss

pixelScale = 5.0

simulationStepsPerSecond = 2

displayWidth = 1500
displayHeight = 1500

window :: Display
window =
  InWindow "Sorting Visualization" (displayWidth, displayHeight) (10, 10)

main :: IO ()
main = do
  putStrLn "hello world"
