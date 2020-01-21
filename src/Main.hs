module Main where

import           Sort
import qualified Data.Set                      as S
import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort
import           Control.Monad.State

pixelScale = 40.0

displayWidth = 1500
displayHeight = 1500

window :: Display
window =
  InWindow "Sorting Visualization" (displayWidth, displayHeight) (10, 10)

simulationStepsPerSecond = 3

goals = S.singleton (Coord 5 5)
startCoord = S.singleton (Coord 0 0)

visualize = visualizePath pixelScale

model :: (Bool, BFS)
model =
  (True, emptyBFS { neighbors = startCoord, start = startCoord, goal = goals })

stepModel :: ViewPort -> Float -> (Bool, BFS) -> (Bool, BFS)
stepModel _ _ (done, bfs) = if done then runState bfsStep bfs else (done, bfs)

main :: IO ()
main = simulate window
                Sort.unseenColor
                simulationStepsPerSecond
                model
                visualize
                stepModel
