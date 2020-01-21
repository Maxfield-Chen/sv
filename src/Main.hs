module Main where

import qualified Sort                          as S
import qualified Data.Map                      as M
import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort
import           Control.Monad.State

pixelScale = 10.0

displayWidth = 1500
displayHeight = 1500

window :: Display
window =
  InWindow "Sorting Visualization" (displayWidth, displayHeight) (10, 10)

unseenColor = greyN 0.9

simulationStepsPerSecond = 3

goal = [S.Coord 5 5]

visualize = S.visualizePath pixelScale

model :: (Bool, S.BFS)
model = (False, S.emptyBFS { S.neighbors = [S.Coord 0 0], S.goal = goal })

stepModel :: ViewPort -> Float -> (Bool, S.BFS) -> (Bool, S.BFS)
stepModel _ _ (done, bfs) =
  if done then (done, bfs) else runState S.bfsStep bfs

main :: IO ()
main =
  simulate window unseenColor simulationStepsPerSecond model visualize stepModel
