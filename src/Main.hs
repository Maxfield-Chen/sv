module Main where

import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort
import qualified Data.Set                      as S
import           Control.Monad.State
import           Search
import           Bfs

pixelScale = 12.0

displayWidth = 1500
displayHeight = 1500

window :: Display
window = InWindow "Search Visualization" (displayWidth, displayHeight) (0, 0)

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
main =
  simulate window unseenColor simulationStepsPerSecond model visualize stepModel
